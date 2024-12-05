!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_xbeach_data
   use precision, only: dp
   use m_xbeach_typesandkinds
   !==================================================================================================================================
   ! XBeach related variables
   !==================================================================================================================================
   !! Hydrodynamics arrays, allocatables
   real(kind=dp), allocatable :: ee0(:, :) !< wave energy at begin of timestep
   real(kind=dp), allocatable :: ee1(:, :) !< wave energy at end of timestep
   real(kind=dp), allocatable :: cwav(:) !< phase speed (m/s)
   real(kind=dp), allocatable :: cwav_s(:) !< phase speed (m/s) single_dir
   real(kind=dp), allocatable :: cgwav(:) !< wave group velocity (m/s)
   real(kind=dp), allocatable :: cgwav_s(:) !< wave group velocity (m/s) single dir
   real(kind=dp), allocatable :: ctheta_s(:, :) !< propagation speed in theta space single dir
   real(kind=dp), allocatable :: ee_s(:, :) !< wave energy single dir
   real(kind=dp), allocatable :: kwav(:) !< wavenumber k (rad/m)
   real(kind=dp), allocatable :: nwav(:) !< cg/c (-)
   real(kind=dp), allocatable :: ctheta(:, :) !< propagation speed in theta space
   real(kind=dp), allocatable :: sigmwav(:) !< wave frequency (rad/s)
   real(kind=dp), allocatable :: sigt(:, :)
   real(kind=dp), allocatable :: horadvec(:, :) !< horizontal advection
   real(kind=dp), allocatable :: thetaadvec(:, :) !< directional advection
   real(kind=dp), allocatable :: rhs(:, :) !< right-hand side
   real(kind=dp), allocatable :: rrthetaadvec(:, :) !< directional advection roller
   real(kind=dp), allocatable :: rrhoradvec(:, :) !< horz advection roller
   real(kind=dp), allocatable :: rr(:, :) !< directional advection roller
   real(kind=dp), allocatable :: csx(:)
   real(kind=dp), allocatable :: snx(:)
   real(kind=dp), allocatable :: H(:) !< hrms golfhoogte, onafh van instat
   real(kind=dp), allocatable :: E(:) !< bulk wave energy in nodes
   real(kind=dp), allocatable :: DR(:) !< Bulk roller dissipation
   real(kind=dp), allocatable :: R(:) !< Bulk roller energy
   real(kind=dp), allocatable :: thet(:, :) !< centre angle dir bin in each node
   real(kind=dp), allocatable :: costh(:, :)
   real(kind=dp), allocatable :: sinth(:, :)
   real(kind=dp), allocatable :: thet_s(:, :)
   real(kind=dp), allocatable :: sinth_s(:, :)
   real(kind=dp), allocatable :: costh_s(:, :)
   real(kind=dp), allocatable :: Sxx(:) !< Radiation stresses
   real(kind=dp), allocatable :: Syy(:)
   real(kind=dp), allocatable :: Sxy(:)
   real(kind=dp), allocatable :: dhsdx(:)
   real(kind=dp), allocatable :: dhsdy(:)
   real(kind=dp), allocatable :: Fx(:) !< wave forces, on links
   real(kind=dp), allocatable :: Fy(:)
   real(kind=dp), allocatable :: Fx_cc(:) !< wave forces in cc, for output
   real(kind=dp), allocatable :: Fy_cc(:)
   real(kind=dp), allocatable :: ust(:) !< Stokes drift east
   real(kind=dp), allocatable :: vst(:) !< Stokes drift north
   real(kind=dp), allocatable :: xbducxdx(:) !< velocity gradients
   real(kind=dp), allocatable :: xbducydx(:) !<
   real(kind=dp), allocatable :: xbducxdy(:) !<
   real(kind=dp), allocatable :: xbducydy(:) !<
   real(kind=dp), allocatable :: dbetadx(:) !< riemann invariant gradients
   real(kind=dp), allocatable :: dbetady(:) !<
   real(kind=dp), allocatable :: sinh2kh(:) !< sinh(2kh)

   real(kind=dp), allocatable :: thetamean(:) !< mean wave angle
   real(kind=dp), allocatable :: Qb(:) !< Wave breaking proportion
   real(kind=dp), allocatable :: D(:) !< Wave breaking dissipation
   real(kind=dp), allocatable :: Df(:) !< Bottom frictional dissipation
   real(kind=dp), allocatable :: Dtot(:)
   real(kind=dp), allocatable :: BR(:) !< Roller surface slope, also important for morph
   real(kind=dp), allocatable :: uin(:) !< xcomponent incoming long wave induced velocity
   real(kind=dp), allocatable :: vin(:) !< ycomponent incoming long wave induced velocity
   real(kind=dp), allocatable :: bi(:) !< long wave component bichromatic bc
   real(kind=dp), allocatable :: ktb(:) !< Short wave induced turbulence near the bottom in flow nodes
   real(kind=dp), allocatable :: Tbore(:) !< Bore period

   real(kind=dp), allocatable :: Ltemp(:)
   real(kind=dp), allocatable :: L1(:)
   real(kind=dp), allocatable :: e01(:)
   real(kind=dp), allocatable :: tE(:), dataE(:), databi(:)
   real(kind=dp), allocatable :: L0(:), khdisp(:), hdisp(:)

   integer :: newstatbc !< stationary bc generated
   real(kind=dp) :: xref0, yref0 !< reference coordinates phase shift bc
   integer, allocatable :: randomseed(:)

   !< absgen bc
   integer :: maxnumbnds = 0
   integer, allocatable, dimension(:) :: kbndu2kbndw !< mapping velocity bc pts to wave bc pts
   integer, allocatable, dimension(:) :: kbndw2kbndu !< mapping velocity bc pts to wave bc pts
   integer, allocatable, dimension(:) :: kbndz2kbndw !< mapping water level bc pts to wave bc pts
   real(kind=dp), allocatable, dimension(:) :: uave !< boundary averaged velocity
   real(kind=dp), allocatable, dimension(:) :: vave !<
   real(kind=dp), allocatable, dimension(:) :: dlengthrm !< boundary length
   real(kind=dp), allocatable, dimension(:) :: umeanrm !< relaxated velocity riemann bnd
   real(kind=dp), allocatable, dimension(:) :: vmeanrm !<
   real(kind=dp), allocatable, dimension(:) :: u1rm !<
   real(kind=dp), allocatable, dimension(:) :: hstokes !<

   !> statsolver administration
   integer, dimension(:, :), allocatable :: connected_nodes
   integer :: no_connected_nodes
   integer, dimension(:), allocatable :: nmmask
   real(kind=dp), dimension(:, :), allocatable :: wmask ! not integer, has weights
   !
   !> statsolver netnode oriented quantities
   integer :: noseapts !< number of offshore wave boundary net nodes
   integer, dimension(:), allocatable :: seapts !< netnodes on wave boundary
   real(kind=dp), dimension(:, :, :), allocatable :: w !< weights of upwind grid points, 2 per grid point and per wave direction
   real(kind=dp), dimension(:, :), allocatable :: ds !< distance to interpolated upwind point, per grid point and direction
   logical, dimension(:), allocatable :: inner !< mask of inner grid points (not on boundary)
   integer, dimension(:, :, :), allocatable :: prev !< two upwind grid points per grid point and wave direction
   real(kind=dp), dimension(:), allocatable :: hhstat !< water depth
   real(kind=dp), dimension(:), allocatable :: kwavstat !< wave number
   real(kind=dp), dimension(:), allocatable :: cgstat !< group velocity
   real(kind=dp), dimension(:), allocatable :: cstat !< phase velocity
   real(kind=dp), dimension(:, :), allocatable :: cthetastat !< refraction speed
   real(kind=dp), dimension(:, :), allocatable :: eestat !< wave energy distribution
   real(kind=dp), dimension(:), allocatable :: Erstat !< bulk roller energy stationary model
   real(kind=dp), dimension(:), allocatable :: fwstat !< wave friction factor
   real(kind=dp), dimension(:), allocatable :: Hstat !< wave height
   real(kind=dp), dimension(:), allocatable :: Dwstat !< wave breaking dissipation
   real(kind=dp), dimension(:), allocatable :: Dfstat !< wave friction dissipation
   real(kind=dp), dimension(:), allocatable :: Drstat !< roller dissipation
   real(kind=dp), dimension(:), allocatable :: thetam !< mean wave direction
   real(kind=dp), dimension(:), allocatable :: uorbstat !< orbital velocity
   real(kind=dp), dimension(:), allocatable :: dhdxstat !< depth gradient, x
   real(kind=dp), dimension(:), allocatable :: dhdystat !< depth gradient, y
   real(kind=dp), dimension(:, :, :), allocatable :: wmean !< weights stationary roller model
   integer, dimension(:, :, :), allocatable :: prevmean !< two upwind grid points per grid point roller model
   real(kind=dp), dimension(:, :), allocatable :: dsmean !< distance to interpolated upwind point, per grid point roller model
   real(kind=dp), dimension(:), allocatable :: Hmaxstat !< Maximum expected wave height in corner point
   integer, dimension(:, :), allocatable :: kp !< computational kernel around all numk net nodes

   !< Relaxated depth and velocities
   real(kind=dp), dimension(:), allocatable :: hhw !< mode dependent water depth
   real(kind=dp), dimension(:), allocatable :: hhws !< depth with relaxation, singledir
   real(kind=dp), dimension(:), allocatable :: ucxws !< ucx with relaxation, singledir
   real(kind=dp), dimension(:), allocatable :: ucyws !< ucy with relaxation, singledir
   real(kind=dp), dimension(:), allocatable :: hhwwci !< depth with relaxation, wci
   real(kind=dp), dimension(:), allocatable :: km !< wave number k with wci
   real(kind=dp), dimension(:), allocatable :: umwci !< ucx with relaxation,  wci
   real(kind=dp), dimension(:), allocatable :: vmwci !< ucx with relaxation,  wci

   !  for plotting
   integer :: itheta_view = 5

   !! Model parameters
   !! 1. DFLOW specific
   real(kind=dp) :: dtmaxwav !< subtimestepping for xbeach wave driver
   real(kind=dp) :: dtmaximp !< pseudotimestepping for implicit wave driver

   integer :: xb_started = 0

   !! 2. Surfbeat specific
   !  Type                    name                          initialize    !  [unit] (advanced/deprecated) description
   ! [Section] Physical processes
   integer :: swave = -123 !  [-] Include short waves (1), exclude short waves (0)
   integer :: lwave = -123 !  [-] Include short wave forcing on NLSW equations and boundary conditions (1), or exclude (0)

   ! [Section] Wave boundary condition parameters
   character(slen) :: instat = 'abc' !  [-] Wave boundary condition type
   real(kind=dp) :: taper = -123 !  [s] Spin-up time of wave boundary conditions, in hydrodynamic time
   real(kind=dp) :: Hrms = -123 !  [m] Hrms wave height for instat = 0,1,2,3
   real(kind=dp) :: Tm01 = -123 !  [s] (deprecated) Old name for Trep
   real(kind=dp) :: Trep = -123 !  [s] Representative wave period for instat = 0,1,2,3
   real(kind=dp) :: Tlong = -123 !  [s] Wave group period for case instat = 1
   real(kind=dp) :: dir0 = -123 !  [deg] Mean wave direction (Nautical convention) for instat = 0,1,2,3
   real(kind=dp) :: nwavmax = -123 !  [-] (advanced) maximum ratio of cg/c fro computing long wave boundary conditions
   integer :: m = -123 !  [-] Power in cos^m directional distribution for instat = 0,1,2,3
   logical :: bccreated = .false. !  [-] Boundary conditions created or not for current run
   integer :: rmfilno = -123 !  [-] debug file id for bc check
   integer :: single_dir = -123 !  [-] switch on single direction wave propagation

   ! [Section] Wave-spectrum boundary condition parameters
   character(slen) :: bcfile = 'abc' !  [-] Name of spectrum file
   integer :: random = -123 !  [-] (advanced) Random seed on (1) or off (0) for instat = 4,5,6 boundary conditions
   real(kind=dp) :: fcutoff = -123 !  [Hz] (advanced) Low-freq cutoff frequency for instat = 4,5,6 boundary conditions
   integer :: nspr = -123 !  [-] (advanced) nspr = 1 long wave direction forced into centres of short wave bins, nspr = 0 regular long wave spreadin
   real(kind=dp) :: trepfac = -123 !  [-] (advanced) Compute mean wave period over energy band: trepfac*maxval(Sf) for instat 4,5,6; converges to Tm01 for trepfac = 0.0 and
   real(kind=dp) :: sprdthr = -123 !  [-] (advanced) Threshold ratio to maxval of S above which spec dens are read in (default 0.08*maxval)
   integer :: correctHm0 = -123 !  [-] (advanced) Turn off or on Hm0 correction
   integer :: Tm01switch = -123 !  [-] (advanced) Turn off or on Tm01 or Tm-10 switch
   real(kind=dp) :: rt = -123 !  [s] Duration of wave spectrum at offshore boundary, in morphological time
   real(kind=dp) :: dtbc = -123 !  [s] (advanced) Timestep used to describe time series of wave energy and long wave flux at offshore boundary (not affected by morfac)
   real(kind=dp) :: dthetaS_XB = -123 !  [deg] (advanced) The (counter-clockwise) angle in the degrees needed to rotate from the x-axis in SWAN to the x-axis pointing East
   integer :: nspectrumloc = -123 !  [-] (advanced) Number of input spectrum locations
   integer :: oldnyq = -123 !  [-] (advanced) Turn off or on old nyquist switch
   real(kind=dp) :: swkhmin = -123 !  [-] (advanced,silent) Minimum kh value to include in wave action balance, lower included in NLSWE (default -1.d0)
   real(kind=dp) :: wbcEvarreduce = -123
   real(kind=dp) :: wbcQvarreduce = -123
   integer :: wbcScaleEnergy = -123
   integer :: wbcRemoveStokes = -123

   ! [Section] Flow boundary condition parameters
   integer :: order = -123 !  [-] (advanced) Switch for order of wave steering, 1 = first order wave steering (short wave energy only), 2 = second oder wave steering (bound long wave corresponding to short wave forcing is added)
   integer :: freewave = -123 !  [-] (advanced) Switch for free wave propagation 0 = use cg (default); 1 = use sqrt(gh) in instat = 3
   real(kind=dp) :: epsi = -123 !  [-] (advanced) Ratio of mean current to time varying current through offshore boundary
   character(slen) :: tidetype = 'abc' !  [-] (advanced) Switch for offshore boundary, velocity boundary or instant water level boundary (default)
   character(slen) :: absgentype = 'abc' !  [-] (advanced) Switch for offshore boundary, 1d flumelike boundary or full 2d absorbing generating bnd
   integer :: ARC = -123 !  [-] (advanced) Switch for active reflection compensation at seaward boundary: 0 = reflective, 1 = weakly (non) reflective
   real(kind=dp) :: hminlw = -123 !  [-] minimum depth for wave forcing in flow momentum equation RHS
   integer :: oldhmin = -123 !
   real(kind=dp) :: deltahmin = -123 !

   ! [Section] Wave breaking parameters
   character(slen) :: break = 'abc' !  [-] Type of breaker formulation
   real(kind=dp) :: gamma = -123 !  [-] Breaker parameter in Baldock or Roelvink formulation
   real(kind=dp) :: gamma2 = -123 !  [-] End of breaking parameter in break = 4 formulation
   real(kind=dp) :: alpha = -123 !  [-] (advanced) Wave dissipation coefficient in Roelvink formulation
   real(kind=dp) :: nroelvink = -123 !  [-] (advanced) Power in Roelvink dissipation model
   real(kind=dp) :: gammaxxb = -123 !  [-] (advanced) Maximum ratio wave height to water depth
   real(kind=dp) :: deltaH = -123 !  [-] (advanced) Fraction of wave height to add to water depth
   real(kind=dp), allocatable :: fw(:) !  [-] (advanced) Internally used bed friction factor
   real(kind=dp) :: fwcutoff = -123 !  [-] Depth greater than which the bed friction factor is NOT applied
   character(slen) :: wavefricfile = 'abc' !  [-] (advanced) Filename spatially varying sample file bed friction factor
   real(kind=dp) :: wavefricval = -123 !  [-] Bed friction factor from params file
   integer :: rollergammax = -123 !  [-] depth limitation of roller energy

   ! [Section] Roller parameters
   integer :: roller = -123 !  [-] (advanced) Turn on (1) or off(0) roller model
   real(kind=dp) :: beta = -123 !  [-] (advanced) Breaker slope coefficient in roller model
   integer :: varbeta = -123 !  [-] (advanced) Rafati varying roller slope
   integer :: rfb = -123 !  [-] (advanced) Switch to feed back maximum wave surface slope in roller energy balance, otherwise rfb = par%Beta
   real(kind=dp) :: nuhfac = -123 !  [-] (advanced) Calibration factor for roller turbulence induced viscosity

   ! [Section] Wave-current interaction parameters
   integer :: wci = -123 !  [-] Turns on (1) or off (0) wave-current interaction
   real(kind=dp) :: hwci = -123 !  [m] (advanced) Minimum depth until which wave-current interaction is used
   real(kind=dp) :: hwcimax = -123 !  [m] (advanced) Maximum depth until which wave-current interaction is used
   real(kind=dp) :: cats = -123 !  [Trep] (advanced) Current averaging time scale for wci, in terms of mean wave periods

   ! [Section] Wave numerics parameters
   real(kind=dp) :: wavint = -123 !  [s] Interval between wave module calls (only in stationary wave mode)
   real(kind=dp) :: maxerror = -123 !  [m] (advanced) Maximum wave height error in wave stationary iteration
   integer :: maxiter = -123 !  [-] (advanced) Maximum number of iterations in wave stationary
   integer :: tsmult = -123 !  [-] multiplier, maximizes implicit timestep based on CFL based timestep for implicit solver
   real(kind=dp) :: waveps = -123 !  [-] eps for wave related quantities, for comparison with XBeach
   real(kind=dp) :: d_relaxfac = -123 !  [-] Relaxation factor for wave dissipation in stationary solver
   !
   ! [Section] Roller and wave turbulence parameters
   real(kind=dp) :: BRfac = -123 !  [-] (advanced) Calibration factor surface slope
   integer :: turb = -123 !  [name] (advanced) Switch to include short wave turbulence
   real(kind=dp) :: Tbfac = -123 !  [-] (advanced) Calibration factor for bore interval Tbore: Tbore = Tbfac*Tbore
   !
   !
   ! [Section] Hydrodynamics for FI (frequency integrated) approach as opposed to FF (fixed frequency)
   !integer                 :: windmodel                  = -123    !   [-] Turns on (1) or off (0) the frequency integrated 2-equation approach
   !integer                 :: advecmod                   = -123    !   [-] advect moments m^E_-1 an m^E_0 (1) or moments m^E_0 and m^E_1
   !real(kind=dp)        :: Trepini                    = -123    !   [s] Initial fill value for Trep in entire domain
   !real(kind=dp)        :: Eini                       = -123    !   [J/rad/m2] Initial fill value for ee1 in entire domain
   !!arrays
   !real(kind=dp), allocatable              :: tt1(:,:)          !   [s] wave period per itheta-bin
   !real(kind=dp), allocatable              :: cwavt(:,:)        !   [m/s] phase speed  per itheta-bin
   !real(kind=dp), allocatable              :: cgwavt(:,:)       !   [m/s] wave group velocity per itheta-bin
   !real(kind=dp), allocatable              :: kwavt(:,:)        !   [rad/m] wavenumber k per itheta-bin
   !real(kind=dp), allocatable              :: nwavt(:,:)        !   [-] cg/c per itheta-bin
   !real(kind=dp), allocatable              :: horadvec2(:,:)    !   [] horizontal advection 2nd moment
   !real(kind=dp), allocatable              :: thetaadvec2(:,:)  !   [] directional advection 2nd moment
   !
   !real(kind=dp), allocatable              :: Ltempt(:,:)       !   [m] wave length temp per itheta-bin
   !real(kind=dp), allocatable              :: L1t(:,:)          !   [m] wave length end per itheta-bin
   !real(kind=dp), allocatable              :: L0t(:,:)          !   [m] wave length start per itheta-bin
   !
   !real(kind=dp), allocatable              :: ma(:,:)           !   [varying] pointer to moment a (depends on advecmod)
   !real(kind=dp), allocatable              :: mb(:,:)           !   [varying] pointer to moment b (depends on advecmod)
   !
   !
   !! [Section] Windmodel source numerics parameters
   !real(kind=dp)        :: mwind                      = -123    !  [-] ideal distribution shape parameter wind source
   !real(kind=dp)        :: ndissip                    = -123    !  [-] wave shape parameter in wavenumber spectrum (Booij (1999))
   !integer                 :: jawsource                  = -123    !  [-] switch wind source term or not
   !integer                 :: jagradcg                   = -123    !  [-] switch include grad(cg) in windsource term
   !real(kind=dp)        :: coefdispT                  = -123    !  [-] taperfactor on wave period dissipation
   !real(kind=dp)        :: coefdispk                  = -123    !  [-] shape factor on wave number limitation on wave period dissipation
   !real(kind=dp)        :: Eful                       = 0.0036d0!  [-] fully developed dimensionless wave energy (Pierson Moskowitz 1964)
   !real(kind=dp)        :: Tful                       = 7.69d0  !  [-] fully developed dimensionless peak period (Pierson Moskowitz 1964)
   !real(kind=dp)        :: aa1                        = 0.00288d0! [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   !real(kind=dp)        :: bb1                        = 0.45d0  !  [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   !real(kind=dp)        :: aa2                        = 0.459d0 !  [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   !real(kind=dp)        :: bb2                        = 0.27d0  !  [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   !real(kind=dp)        :: CE1                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   !real(kind=dp)        :: CE2                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   !real(kind=dp)        :: CT1                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   !real(kind=dp)        :: CT2                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   !! arrays
   !real(kind=dp), allocatable              :: wmagcc(:)         !  [m/s] wind speed magnitude cell centered
   !real(kind=dp), allocatable              :: windspreadfac(:,:)!  [-] distribution of inproducts thetabins per cell with wind direction
   !real(kind=dp), allocatable              :: SwE(:)            !  [-] nodal wind source term energy
   !real(kind=dp), allocatable              :: SwT(:)            !  [-] nodal wind source term period
   !real(kind=dp), allocatable              :: wsorE(:,:)        !  [J/m2/s] wind source term for ee1
   !real(kind=dp), allocatable              :: wsorT(:,:)        !  [s/s] wind source term for tt1
   !real(kind=dp), allocatable              :: egradcg(:,:)      !  [m/s/m] spatial gradient of cg
   !real(kind=dp), allocatable              :: ddT(:)            !  [s/s] dissipation of wave period
end module m_xbeach_data
