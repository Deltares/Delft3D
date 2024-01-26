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
   use m_xbeach_typesandkinds
   !==================================================================================================================================
   ! XBeach related variables
   !==================================================================================================================================
   !! Hydrodynamics arrays, allocatables
   double precision, allocatable              :: ee0(:,:)        !< wave energy at begin of timestep
   double precision, allocatable              :: ee1(:,:)        !< wave energy at end of timestep
   double precision, allocatable              :: cwav(:)         !< phase speed (m/s)
   double precision, allocatable              :: cwav_s(:)       !< phase speed (m/s) single_dir
   double precision, allocatable              :: cgwav(:)        !< wave group velocity (m/s)
   double precision, allocatable              :: cgwav_s(:)      !< wave group velocity (m/s) single dir
   double precision, allocatable              :: ctheta_s(:,:)   !< propagation speed in theta space single dir
   double precision, allocatable              :: ee_s(:,:)       !< wave energy single dir
   double precision, allocatable              :: kwav(:)         !< wavenumber k (rad/m)
   double precision, allocatable              :: nwav(:)         !< cg/c (-)
   double precision, allocatable              :: ctheta(:,:)     !< propagation speed in theta space
   double precision, allocatable              :: sigmwav(:)      !< wave frequency (rad/s)
   double precision, allocatable              :: sigt(:,:)
   double precision, allocatable              :: horadvec(:,:)   !< horizontal advection
   double precision, allocatable              :: thetaadvec(:,:) !< directional advection
   double precision, allocatable              :: rhs(:,:)        !< right-hand side
   double precision, allocatable              :: rrthetaadvec(:,:) !< directional advection roller
   double precision, allocatable              :: rrhoradvec(:,:) !< horz advection roller
   double precision, allocatable              :: rr(:,:)         !< directional advection roller
   double precision, allocatable              :: csx(:)
   double precision, allocatable              :: snx(:)
   double precision, allocatable              :: H(:)            !< hrms golfhoogte, onafh van instat
   double precision, allocatable              :: E(:)            !< bulk wave energy in nodes
   double precision, allocatable              :: DR(:)           !< Bulk roller dissipation
   double precision, allocatable              :: R(:)            !< Bulk roller energy
   double precision, allocatable              :: thet(:,:)       !< centre angle dir bin in each node
   double precision, allocatable              :: costh(:,:)
   double precision, allocatable              :: sinth(:,:)
   double precision, allocatable              :: thet_s(:,:)
   double precision, allocatable              :: sinth_s(:,:)
   double precision, allocatable              :: costh_s(:,:)
   double precision, allocatable              :: Sxx(:)          !< Radiation stresses
   double precision, allocatable              :: Syy(:)
   double precision, allocatable              :: Sxy(:)
   double precision, allocatable              :: dhsdx(:)
   double precision, allocatable              :: dhsdy(:)
   double precision, allocatable              :: Fx(:)           !< wave forces, on links
   double precision, allocatable              :: Fy(:)
   double precision, allocatable              :: Fx_cc(:)        !< wave forces in cc, for output
   double precision, allocatable              :: Fy_cc(:)
   double precision, allocatable              :: ust(:)          !< Stokes drift east
   double precision, allocatable              :: vst(:)          !< Stokes drift north
   double precision, allocatable              :: xbducxdx(:)     !< velocity gradients
   double precision, allocatable              :: xbducydx(:)     !<
   double precision, allocatable              :: xbducxdy(:)     !<
   double precision, allocatable              :: xbducydy(:)     !<
   double precision, allocatable              :: dbetadx(:)      !< riemann invariant gradients
   double precision, allocatable              :: dbetady(:)      !<
   double precision, allocatable              :: sinh2kh(:)      !< sinh(2kh)

   double precision, allocatable              :: thetamean(:)    !< mean wave angle
   double precision, allocatable              :: Qb(:)           !< Wave breaking proportion
   double precision, allocatable              :: D(:)            !< Wave breaking dissipation
   double precision, allocatable              :: Df(:)           !< Bottom frictional dissipation
   double precision, allocatable              :: Dtot(:)
   double precision, allocatable              :: BR(:)           !< Roller surface slope, also important for morph
   double precision, allocatable              :: uin(:)          !< xcomponent incoming long wave induced velocity
   double precision, allocatable              :: vin(:)          !< ycomponent incoming long wave induced velocity
   double precision, allocatable              :: bi(:)           !< long wave component bichromatic bc
   double precision, allocatable              :: ktb(:)          !< Short wave induced turbulence near the bottom in flow nodes
   double precision, allocatable              :: Tbore(:)        !< Bore period

   double precision, allocatable              :: Ltemp(:)
   double precision, allocatable              :: L1(:)
   double precision, allocatable              :: e01(:)
   double precision, allocatable              :: tE(:), dataE(:), databi(:)
   double precision, allocatable              :: L0(:),khdisp(:),hdisp(:)

   integer                                    :: newstatbc       !< stationary bc generated
   double precision                           :: xref0, yref0    !< reference coordinates phase shift bc
   integer         , allocatable              :: randomseed(:)

   !< absgen bc
   integer                                     :: maxnumbnds = 0
   integer,          allocatable, dimension(:) :: kbndu2kbndw    !< mapping velocity bc pts to wave bc pts
   integer,          allocatable, dimension(:) :: kbndw2kbndu    !< mapping velocity bc pts to wave bc pts
   integer,          allocatable, dimension(:) :: kbndz2kbndw    !< mapping water level bc pts to wave bc pts
   double precision, allocatable, dimension(:) :: uave           !< boundary averaged velocity
   double precision, allocatable, dimension(:) :: vave           !<
   double precision, allocatable, dimension(:) :: dlengthrm      !< boundary length
   double precision, allocatable, dimension(:) :: umeanrm        !< relaxated velocity riemann bnd
   double precision, allocatable, dimension(:) :: vmeanrm        !<
   double precision, allocatable, dimension(:) :: u1rm           !<
   double precision, allocatable, dimension(:) :: hstokes        !<

   !> statsolver administration
   integer, dimension(:,:), allocatable           :: connected_nodes
   integer                                        :: no_connected_nodes
   integer, dimension(:), allocatable             :: nmmask
   double precision, dimension(:,:), allocatable  :: wmask                  ! not integer, has weights
   !
   !> statsolver netnode oriented quantities
   integer                                                  :: noseapts     !< number of offshore wave boundary net nodes
   integer         , dimension(:)     , allocatable         :: seapts       !< netnodes on wave boundary
   double precision, dimension(:,:,:) , allocatable         :: w            !< weights of upwind grid points, 2 per grid point and per wave direction
   double precision, dimension(:,:)   , allocatable         :: ds           !< distance to interpolated upwind point, per grid point and direction
   logical         , dimension(:)     , allocatable         :: inner        !< mask of inner grid points (not on boundary)
   integer         , dimension(:,:,:) , allocatable         :: prev         !< two upwind grid points per grid point and wave direction
   double precision, dimension(:)     , allocatable         :: hhstat       !< water depth
   double precision, dimension(:)     , allocatable         :: kwavstat     !< wave number
   double precision, dimension(:)     , allocatable         :: cgstat       !< group velocity
   double precision, dimension(:)     , allocatable         :: cstat        !< phase velocity
   double precision, dimension(:,:)   , allocatable         :: cthetastat   !< refraction speed
   double precision, dimension(:,:)   , allocatable         :: eestat       !< wave energy distribution
   double precision, dimension(:)     , allocatable         :: Erstat       !< bulk roller energy stationary model
   double precision, dimension(:)     , allocatable         :: fwstat       !< wave friction factor
   double precision, dimension(:)     , allocatable         :: Hstat        !< wave height
   double precision, dimension(:)     , allocatable         :: Dwstat       !< wave breaking dissipation
   double precision, dimension(:)     , allocatable         :: Dfstat       !< wave friction dissipation
   double precision, dimension(:)     , allocatable         :: Drstat       !< roller dissipation
   double precision, dimension(:)     , allocatable         :: thetam       !< mean wave direction
   double precision, dimension(:)     , allocatable         :: uorbstat     !< orbital velocity
   double precision, dimension(:)     , allocatable         :: dhdxstat     !< depth gradient, x
   double precision, dimension(:)     , allocatable         :: dhdystat     !< depth gradient, y
   double precision, dimension(:,:,:) , allocatable         :: wmean        !< weights stationary roller model
   integer         , dimension(:,:,:) , allocatable         :: prevmean     !< two upwind grid points per grid point roller model
   double precision, dimension(:,:)   , allocatable         :: dsmean       !< distance to interpolated upwind point, per grid point roller model
   double precision, dimension(:)     , allocatable         :: Hmaxstat     !< Maximum expected wave height in corner point
   integer         , dimension(:,:)   , allocatable         :: kp           !< computational kernel around all numk net nodes

   !< Relaxated depth and velocities
   double precision, dimension(:)     , allocatable         :: hhw          !< mode dependent water depth
   double precision, dimension(:)     , allocatable         :: hhws         !< depth with relaxation, singledir
   double precision, dimension(:)     , allocatable         :: ucxws        !< ucx with relaxation, singledir
   double precision, dimension(:)     , allocatable         :: ucyws        !< ucy with relaxation, singledir
   double precision, dimension(:)     , allocatable         :: hhwwci       !< depth with relaxation, wci
   double precision, dimension(:)     , allocatable         :: km           !< wave number k with wci
   double precision, dimension(:)     , allocatable         :: umwci        !< ucx with relaxation,  wci
   double precision, dimension(:)     , allocatable         :: vmwci        !< ucx with relaxation,  wci

   !  for plotting
   integer                                     :: itheta_view=5

   !! Model parameters
   !! 1. DFLOW specific
   double precision                           :: dtmaxwav        !< subtimestepping for xbeach wave driver
   double precision                           :: dtmaximp        !< pseudotimestepping for implicit wave driver

   integer                                    :: xb_started=0

   !! 2. Surfbeat specific
   !  Type                    name                          initialize    !  [unit] (advanced/deprecated) description
   ! [Section] Physical processes
   integer                 :: swave                      = -123    !  [-] Include short waves (1), exclude short waves (0)
   integer                 :: lwave                      = -123    !  [-] Include short wave forcing on NLSW equations and boundary conditions (1), or exclude (0)

   ! [Section] Wave boundary condition parameters
   character(slen)         :: instat                     = 'abc'   !  [-] Wave boundary condition type
   double precision        :: taper                      = -123    !  [s] Spin-up time of wave boundary conditions, in hydrodynamic time
   double precision        :: Hrms                       = -123    !  [m] Hrms wave height for instat = 0,1,2,3
   double precision        :: Tm01                       = -123    !  [s] (deprecated) Old name for Trep
   double precision        :: Trep                       = -123    !  [s] Representative wave period for instat = 0,1,2,3
   double precision        :: Tlong                      = -123    !  [s] Wave group period for case instat = 1
   double precision        :: dir0                       = -123    !  [deg] Mean wave direction (Nautical convention) for instat = 0,1,2,3
   double precision        :: nwavmax                    = -123    !  [-] (advanced) maximum ratio of cg/c fro computing long wave boundary conditions
   integer                 :: m                          = -123    !  [-] Power in cos^m directional distribution for instat = 0,1,2,3
   logical                 :: bccreated                  = .false. !  [-] Boundary conditions created or not for current run
   integer                 :: rmfilno                    = -123    !  [-] debug file id for bc check
   integer                 :: single_dir                 = -123    !  [-] switch on single direction wave propagation

   ! [Section] Wave-spectrum boundary condition parameters
   character(slen)         :: bcfile                     = 'abc'   !  [-] Name of spectrum file
   integer                 :: random                     = -123    !  [-] (advanced) Random seed on (1) or off (0) for instat = 4,5,6 boundary conditions
   double precision        :: fcutoff                    = -123    !  [Hz] (advanced) Low-freq cutoff frequency for instat = 4,5,6 boundary conditions
   integer                 :: nspr                       = -123    !  [-] (advanced) nspr = 1 long wave direction forced into centres of short wave bins, nspr = 0 regular long wave spreadin
   double precision        :: trepfac                    = -123    !  [-] (advanced) Compute mean wave period over energy band: trepfac*maxval(Sf) for instat 4,5,6; converges to Tm01 for trepfac = 0.0 and
   double precision        :: sprdthr                    = -123    !  [-] (advanced) Threshold ratio to maxval of S above which spec dens are read in (default 0.08*maxval)
   integer                 :: correctHm0                 = -123    !  [-] (advanced) Turn off or on Hm0 correction
   integer                 :: Tm01switch                 = -123    !  [-] (advanced) Turn off or on Tm01 or Tm-10 switch
   double precision        :: rt                         = -123    !  [s] Duration of wave spectrum at offshore boundary, in morphological time
   double precision        :: dtbc                       = -123    !  [s] (advanced) Timestep used to describe time series of wave energy and long wave flux at offshore boundary (not affected by morfac)
   double precision        :: dthetaS_XB                 = -123    !  [deg] (advanced) The (counter-clockwise) angle in the degrees needed to rotate from the x-axis in SWAN to the x-axis pointing East
   integer                 :: nspectrumloc               = -123    !  [-] (advanced) Number of input spectrum locations
   integer                 :: oldnyq                     = -123    !  [-] (advanced) Turn off or on old nyquist switch
   double precision        :: swkhmin                    = -123    !  [-] (advanced,silent) Minimum kh value to include in wave action balance, lower included in NLSWE (default -1.d0)
   double precision        :: wbcEvarreduce              = -123
   double precision        :: wbcQvarreduce              = -123
   integer                 :: wbcScaleEnergy             = -123
   integer                 :: wbcRemoveStokes            = -123
   
   ! [Section] Flow boundary condition parameters
   integer                 :: order                      = -123    !  [-] (advanced) Switch for order of wave steering, 1 = first order wave steering (short wave energy only), 2 = second oder wave steering (bound long wave corresponding to short wave forcing is added)
   integer                 :: freewave                   = -123    !  [-] (advanced) Switch for free wave propagation 0 = use cg (default); 1 = use sqrt(gh) in instat = 3
   double precision        :: epsi                       = -123    !  [-] (advanced) Ratio of mean current to time varying current through offshore boundary
   character(slen)         :: tidetype                   = 'abc'   !  [-] (advanced) Switch for offshore boundary, velocity boundary or instant water level boundary (default)
   character(slen)         :: absgentype                 = 'abc'   !  [-] (advanced) Switch for offshore boundary, 1d flumelike boundary or full 2d absorbing generating bnd
   integer                 :: ARC                        = -123    !  [-] (advanced) Switch for active reflection compensation at seaward boundary: 0 = reflective, 1 = weakly (non) reflective
   double precision        :: hminlw                     = -123    !  [-] minimum depth for wave forcing in flow momentum equation RHS
   integer                 :: oldhmin                    = -123    !
   double precision        :: deltahmin                  = -123    !

   ! [Section] Wave breaking parameters
   character(slen)                :: break               = 'abc'   !  [-] Type of breaker formulation
   double precision               :: gamma               = -123    !  [-] Breaker parameter in Baldock or Roelvink formulation
   double precision               :: gamma2              = -123    !  [-] End of breaking parameter in break = 4 formulation
   double precision               :: alpha               = -123    !  [-] (advanced) Wave dissipation coefficient in Roelvink formulation
   double precision               :: nroelvink           = -123    !  [-] (advanced) Power in Roelvink dissipation model
   double precision               :: gammaxxb            = -123    !  [-] (advanced) Maximum ratio wave height to water depth
   double precision               :: deltaH              = -123    !  [-] (advanced) Fraction of wave height to add to water depth
   double precision, allocatable  :: fw(:)                         !  [-] (advanced) Internally used bed friction factor
   double precision               :: fwcutoff            = -123    !  [-] Depth greater than which the bed friction factor is NOT applied
   character(slen)                :: wavefricfile        = 'abc'   !  [-] (advanced) Filename spatially varying sample file bed friction factor
   double precision               :: wavefricval         = -123    !  [-] Bed friction factor from params file
   integer                        :: rollergammax        = -123    !  [-] depth limitation of roller energy

   ! [Section] Roller parameters
   integer                 :: roller                     = -123    !  [-] (advanced) Turn on (1) or off(0) roller model
   double precision        :: beta                       = -123    !  [-] (advanced) Breaker slope coefficient in roller model
   integer                 :: varbeta                    = -123    !  [-] (advanced) Rafati varying roller slope
   integer                 :: rfb                        = -123    !  [-] (advanced) Switch to feed back maximum wave surface slope in roller energy balance, otherwise rfb = par%Beta
   double precision        :: nuhfac                     = -123    !  [-] (advanced) Calibration factor for roller turbulence induced viscosity

   ! [Section] Wave-current interaction parameters
   integer                 :: wci                        = -123    !  [-] Turns on (1) or off (0) wave-current interaction
   double precision        :: hwci                       = -123    !  [m] (advanced) Minimum depth until which wave-current interaction is used
   double precision        :: hwcimax                    = -123    !  [m] (advanced) Maximum depth until which wave-current interaction is used
   double precision        :: cats                       = -123    !  [Trep] (advanced) Current averaging time scale for wci, in terms of mean wave periods

   ! [Section] Wave numerics parameters
   double precision        :: wavint                     = -123    !  [s] Interval between wave module calls (only in stationary wave mode)
   double precision        :: maxerror                   = -123    !  [m] (advanced) Maximum wave height error in wave stationary iteration
   integer                 :: maxiter                    = -123    !  [-] (advanced) Maximum number of iterations in wave stationary
   integer                 :: tsmult                     = -123    !  [-] multiplier, maximizes implicit timestep based on CFL based timestep for implicit solver
   double precision        :: waveps                     = -123    !  [-] eps for wave related quantities, for comparison with XBeach
   double precision        :: d_relaxfac                 = -123    !  [-] Relaxation factor for wave dissipation in stationary solver
   !
   ! [Section] Roller and wave turbulence parameters
   double precision        :: BRfac                      = -123    !  [-] (advanced) Calibration factor surface slope
   integer                 :: turb                       = -123    !  [name] (advanced) Switch to include short wave turbulence
   double precision        :: Tbfac                      = -123    !  [-] (advanced) Calibration factor for bore interval Tbore: Tbore = Tbfac*Tbore
   !
   !
   ! [Section] Hydrodynamics for FI (frequency integrated) approach as opposed to FF (fixed frequency)
   !integer                 :: windmodel                  = -123    !   [-] Turns on (1) or off (0) the frequency integrated 2-equation approach
   !integer                 :: advecmod                   = -123    !   [-] advect moments m^E_-1 an m^E_0 (1) or moments m^E_0 and m^E_1
   !double precision        :: Trepini                    = -123    !   [s] Initial fill value for Trep in entire domain
   !double precision        :: Eini                       = -123    !   [J/rad/m2] Initial fill value for ee1 in entire domain
   !!arrays
   !double precision, allocatable              :: tt1(:,:)          !   [s] wave period per itheta-bin
   !double precision, allocatable              :: cwavt(:,:)        !   [m/s] phase speed  per itheta-bin
   !double precision, allocatable              :: cgwavt(:,:)       !   [m/s] wave group velocity per itheta-bin
   !double precision, allocatable              :: kwavt(:,:)        !   [rad/m] wavenumber k per itheta-bin
   !double precision, allocatable              :: nwavt(:,:)        !   [-] cg/c per itheta-bin
   !double precision, allocatable              :: horadvec2(:,:)    !   [] horizontal advection 2nd moment
   !double precision, allocatable              :: thetaadvec2(:,:)  !   [] directional advection 2nd moment
   !
   !double precision, allocatable              :: Ltempt(:,:)       !   [m] wave length temp per itheta-bin
   !double precision, allocatable              :: L1t(:,:)          !   [m] wave length end per itheta-bin
   !double precision, allocatable              :: L0t(:,:)          !   [m] wave length start per itheta-bin
   !
   !double precision, allocatable              :: ma(:,:)           !   [varying] pointer to moment a (depends on advecmod)
   !double precision, allocatable              :: mb(:,:)           !   [varying] pointer to moment b (depends on advecmod)
   !
   !
   !! [Section] Windmodel source numerics parameters
   !double precision        :: mwind                      = -123    !  [-] ideal distribution shape parameter wind source
   !double precision        :: ndissip                    = -123    !  [-] wave shape parameter in wavenumber spectrum (Booij (1999))
   !integer                 :: jawsource                  = -123    !  [-] switch wind source term or not
   !integer                 :: jagradcg                   = -123    !  [-] switch include grad(cg) in windsource term
   !double precision        :: coefdispT                  = -123    !  [-] taperfactor on wave period dissipation
   !double precision        :: coefdispk                  = -123    !  [-] shape factor on wave number limitation on wave period dissipation
   !double precision        :: Eful                       = 0.0036d0!  [-] fully developed dimensionless wave energy (Pierson Moskowitz 1964)
   !double precision        :: Tful                       = 7.69d0  !  [-] fully developed dimensionless peak period (Pierson Moskowitz 1964)
   !double precision        :: aa1                        = 0.00288d0! [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   !double precision        :: bb1                        = 0.45d0  !  [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   !double precision        :: aa2                        = 0.459d0 !  [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   !double precision        :: bb2                        = 0.27d0  !  [-] shape parameter wave growth curves (Kahma Calkoen (1992))
   !double precision        :: CE1                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   !double precision        :: CE2                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   !double precision        :: CT1                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   !double precision        :: CT2                        = -123    !  [-] wind source term parameter (MSc thesis MvdL)
   !! arrays
   !double precision, allocatable              :: wmagcc(:)         !  [m/s] wind speed magnitude cell centered
   !double precision, allocatable              :: windspreadfac(:,:)!  [-] distribution of inproducts thetabins per cell with wind direction
   !double precision, allocatable              :: SwE(:)            !  [-] nodal wind source term energy
   !double precision, allocatable              :: SwT(:)            !  [-] nodal wind source term period
   !double precision, allocatable              :: wsorE(:,:)        !  [J/m2/s] wind source term for ee1
   !double precision, allocatable              :: wsorT(:,:)        !  [s/s] wind source term for tt1
   !double precision, allocatable              :: egradcg(:,:)      !  [m/s/m] spatial gradient of cg
   !double precision, allocatable              :: ddT(:)            !  [s/s] dissipation of wave period
end module m_xbeach_data
