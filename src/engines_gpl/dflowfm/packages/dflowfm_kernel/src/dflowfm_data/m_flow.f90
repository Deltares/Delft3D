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

module m_flow ! flow arrays-999
   use m_flowparameters
   use fm_external_forcings_data
   use m_flowoutput
   use m_physcoef
   use m_turbulence
   use m_grw
   use m_heatfluxes
   use m_alloc
   use m_vegetation
   use m_ship
   use precision, only: sp

   implicit none

   ! 3D parameters
   integer :: kmx !< nr of 3d layers, increasing in positive upward direction
                                                        !! if kmx==0 then 2D code. if kmx==1 then 3D code
   integer :: kmx1 !< kmx + 1, for dimensioning arrays that used to be (0:kmax)
   integer :: kmxd !< dim of kmx, >= 1
   integer :: ndkx !< dim of 3d flow nodes (internal + boundary)
   integer :: ndkx1 !< dim of 3d flow horizontal interfaces (internal + boundary), (0:kmx)
   integer :: lnkx !< dim of 3d flow links (internal + boundary)
   integer :: numvertdis !< number of           vertical layer distributions
   integer :: mxlayz !< max nr of z     layers in flow domain
   integer :: mxlays !< max nr of sigma layers in flow domain
   integer :: kplot !< layer nr to be plotted
   integer :: nplot !< vertical profile to be plotted at node nr
   integer :: kplotfrombedorsurface = 2 !< up or down k
   integer :: kplotordepthaveraged = 1 !< 1 = kplot, 2 = averaged
   integer :: layertype !< 1= all sigma, 2 = all z, 3 = left sigma, 4 = left z
   integer :: numtopsig = 0 !< number of top layers in sigma
   integer :: janumtopsiguniform = 1 !< specified nr of top layers in sigma is same everywhere

   double precision :: Tsigma = 100 !< relaxation period density controlled sigma
   integer, parameter :: LAYTP_SIGMA = 1
   integer, parameter :: LAYTP_Z = 2
   integer, parameter :: LAYTP_LEFTSIGMA = 3
   integer, parameter :: LAYTP_LEFTZ = 4

   integer :: iStrchType = -1 !< Stretching type for non-uniform layers, 1=user defined, 2=exponential, otherwise=uniform
   integer, parameter :: STRCH_USER = 1
   integer, parameter :: STRCH_EXPONENT = 2

   integer :: iturbulencemodel !< 0=no, 1 = constant, 2 = algebraic, 3 = k-eps
   integer :: ieps !< bottom boundary type eps. eqation, 1=dpmorg, 2 = dpmsandpit, 3=D3D, 4=Dirichlethdzb
   integer :: jadrhodz = 1
   double precision :: turbulence_lax_factor = 0 !< LAX-scheme factor (0.0 - 1.0) for turbulent quantities (0.0: flow links, 0.5: fifty-fifty, 1.0: flow nodes)
   integer :: turbulence_lax_vertical = 1 !< Vertical distribution of turbulence_lax_factor (1: linear increasing from 0.0 to 1.0 in top half only, 2: uniform 1.0 over vertical)
   integer :: turbulence_lax_horizontal = 2 !< Horizontal method of turbulence_lax_factor (1: apply to all cells, 2: only when vertical layers are horizontally connected)
   double precision :: sigmagrowthfactor !<layer thickness growth factor from bed up
   double precision :: dztopuniabovez = -999d0 !< bottom level of lowest uniform layer == blmin if not specified
   double precision :: Floorlevtoplay = -999d0 !< floor  level of top zlayer, == sini if not specified
   double precision :: dztop = -999d0 !< if specified, dz of top layer, kmx = computed, if not, dz = (ztop-zbot)/kmx
   integer :: jaorgFloorlevtoplaydef = 0 !< 0=correct floorlevtoplay, 1 = org wrong floorlevtoplay
   double precision :: zlaybot = -999d0 !< if specified, first zlayer starts from zlaybot, if not, it starts from the lowest bed point
   double precision :: zlaytop = -999d0 !< if specified, highest zlayer ends at zlaytop, if not, it ends at the initial water level
   double precision, allocatable :: aak(:) !< coefficient vertical mom exchange of kmx layers
   double precision, allocatable :: bbk(:) !< coefficient vertical mom exchange of kmx layers
   double precision, allocatable :: cck(:) !< coefficient vertical mom exchange of kmx layers
   double precision, allocatable :: ddk(:) !< coefficient vertical mom exchange of kmx layers
   double precision, allocatable :: eek(:) !< coefficient vertical mom exchange of kmx layers
   double precision, allocatable :: uuk(:) !< coefficient vertical mom exchange of kmx layers

   double precision, allocatable :: laycof(:) !< coefficients for sigma layer
   !    1: Percentages of the layers, user defined, laycof(kmx)
   !    2: Stretching level, and two coefficients for layers growth, laycof(3)
   !
   double precision, allocatable :: dzslay(:, :) ! the normalized thickness of layer, dim = (: , maxlaydefs)

   !double precision, allocatable     :: dzu(:)           !< vertical layer size at layer centre    at u-velocity points (m) 1:kmx Local
   !double precision, allocatable     :: dzw(:)           !< vertical layer size at layer interface at u-velocity points (m) 1:kmx Local
   !< 1:kmx: bottom interface not included

   double precision, allocatable, target :: zws(:) !< [m] z levels  (m) of interfaces (w-points) at cell centres (s-points) (m)    (1:ndkx) {"shape": ["ndkx"]}
   double precision, allocatable :: zws0(:) !< z levels  (m) of interfaces (w-points) at cell centres (s-points) (m)    (1:ndkx), be

                                                        !!
                                                        !!-------------------------------------  zws(2) = interface(2), zws (ktop(ndx) ) == s1(ndx)
                                                        !!                     |
                                                        !!                     |
                                                        !!          +          |                          layer (2)
                                                        !!                     |
                                                        !!                     |
                                                        !!-------------------------------------  zws(1) = interface(1)
                                                        !!                     |
                                                        !!                     |
                                                        !!          +          |                          layer (1)
                                                        !!                     |
                                                        !!                     |
                                                        !!-------------------------------------  zws(0) = interface(0) = bl
                                                        !!

   double precision, allocatable, target :: zcs(:) !< z levels at layer mid-points, only for nudging

   !< [m] waterlevel    (m ) at start of timestep {"location": "face", "shape": ["ndx"]}
   integer, allocatable, target :: kbot(:) !< [-] layer-compressed bottom layer cell number: for each of ndx horizontal cells, we have indices to bot and top ndxk cells {"location": "face", "shape": ["ndx"]}
   integer, allocatable, target :: ktop(:) !< [-] layer-compressed top layer cell number: for each of ndx horizontal cells, we have indices to bot and top ndxk cells {"location": "face", "shape": ["ndx"]}
   integer, allocatable :: ktop0(:) !< store of ktop
   integer, allocatable :: kmxn(:) !< max nr of vertical cells per base cell n
   integer, allocatable, target :: Lbot(:) !< [-] layer-compressed bottom layer edge number: for each of lnx horizontal links, we have indices to bot and top lnxk links {"location": "edge", "shape": ["lnx"]}
   integer, allocatable, target :: Ltop(:) !< [-] layer-compressed top layer edge number: for each of lnx horizontal links, we have indices to bot and top lnxk links {"location": "edge", "shape": ["lnx"]}
   integer, allocatable :: kmxL(:) !< max nr of vertical links per base link L
   integer, allocatable :: kbotc(:) !< as kbot, for cornerpoints
   integer, allocatable :: kmxc(:) !< as kmxn, for cornerpoints

   integer :: mxlaydefs = 4 !< max nr of layering definitions
   integer, allocatable :: laydefnr(:) !< dim = (ndx), pointer to laydef, if positive to unique laydef, otherwise interpolate in 1,2, and 3
   integer, allocatable :: laytyp(:) !< dim = (mxlaydefs), 1 = sigma, 2 = z
   integer, allocatable :: laymx(:) !< dim = (mxlaydefs), max nr of layers
   integer, allocatable :: nrlayn(:) !< dim = (ndx), max nr of layers
   integer, allocatable :: nlaybn(:) !< dim = (ndx), bed lay nr
   double precision, allocatable :: zslay(:, :) !< dim = (: , maxlaydefs) z or s coordinate,
   double precision, allocatable :: wflaynod(:, :) !< dim = (3 , ndx) weight factors to flownodes indlaynod
   integer, allocatable :: indlaynod(:, :) !< dim = (3 , ndx)
   double precision, allocatable :: dkx(:) !< dim = ndx, density controlled sigma, sigma level of interface height
   double precision, allocatable :: sdkx(:) !< dim = ndx, density controlled sigma, sum of .., only layertype == 4

   double precision, allocatable :: asig(:) !< alfa of sigma at nodes, 1d0=full sigma, 0d0=full z, 0.5d0=fifty/fifty
   double precision, allocatable :: ustb(:) !< ustar at Lbot, dim=Lnx,
   double precision, allocatable :: ustw(:) !< ustar at Ltop, dim=Lnx
   double precision, allocatable :: ustbc(:) !< ustar at bed at netnodes, dim=numk

   integer :: nfixed, nsigma

   ! flow arrays

   ! node related, dim = ndx
   double precision, allocatable, target :: s0(:) !< [m] waterlevel    (m ) at start of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: s1(:) !< [m] waterlevel    (m ) at end   of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: s1max(:) !< [m] maximum waterlevel (m ) at end   of timestep for Fourier output {"location": "face", "shape": ["ndx"]}
   double precision, allocatable :: s00(:) !< waterlevel    (m ) for checking iteration in nonlin
   double precision, allocatable, target :: a0(:) !< [m2] storage area at start of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: a1(:) !< [m2] storage area at end of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: vol1(:) !< [m3] total volume at end of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: vol0(:) !< [m3] total volume at start of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: vol1_f(:) !< [m3] flow volume volume at end of timestep {"location": "face", "shape": ["ndx"]}
   double precision, allocatable :: sq(:) !< total  influx (m3/s) at water level point
   double precision, allocatable :: sqa(:) !< total  out! flux (m3/s) at s point, u1 based, non-conservative for iadvec == 38
   double precision, allocatable, target :: hs(:) !< [m] waterdepth at cell centre = s1 - bl  (m) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable :: cfs(:) !< dimensionless friction coefficient sag/C in cell centre
   double precision, allocatable :: volerror(:) !< volume error

   double precision, allocatable :: voldhu(:) !< node volume based on downwind hu

   double precision, allocatable :: s1m(:) !< waterlevel   pressurized nonlin minus part
   double precision, allocatable :: a1m(:) !< surface area pressurized nonlin minus part

   double precision, allocatable :: negativeDepths(:) !< Number of negative depths during output interval at nodes.
   double precision, allocatable :: negativeDepths_cum(:) !< Cumulative number of negative depths at nodes.
   double precision, allocatable :: noIterations(:) !< Number of no iteration locations during output interval at nodes.
   double precision, allocatable :: noIterations_cum(:) !< Cumulative number of no iteration locations at nodes.
   double precision, allocatable :: limitingTimestepEstimation(:) !< Number of times during the output interval the conditions in a node is limiting the time step
   double precision, allocatable :: limitingTimestepEstimation_cum(:) !< Cumulative number of times the conditions in a node is limiting the time step.
   !< Note: this doubles with variable numlimdt(:), which contains the same cumulative count, under a different MDU option.
   !< Note: these variables are double precision (in stead of integers) because post processing is
   !<       based on double precision variables.
   double precision, allocatable :: flowCourantNumber(:) !< Courant number

! node related, dim = ndkx

   double precision, allocatable :: volau(:) !< trial, au based cell volume (m3)
   double precision, allocatable, target :: ucx(:) !< [m/s] cell center velocity, global x-dir (m/s) {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable, target :: ucy(:) !< [m/s] cell center velocity, global y-dir (m/s) {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable, target :: ucz(:) !< [m/s] cell center velocity, global z-dir (m/s) {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable, target :: ucxq(:) !< cell center velocity, q based  global x-dir (m/s)
   double precision, allocatable, target :: ucyq(:) !< cell center velocity, q based  global y-dir (m/s)
   double precision, allocatable :: uqcx(:) !< cell center incoming momentum, global x-dir (m4/s2), only for iadvec = 1
   double precision, allocatable :: uqcy(:) !< cell center incoming momentum, global y-dir (m4/s2), only for iadvec = 1
   double precision, allocatable, target :: ucmag(:) !< [m/s] cell center velocity magnitude {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable :: uc1D(:) !< [m/s] 1D cell center velocities
   double precision, allocatable :: alpha_mom_1D(:) !< [-] ratio of incoming momentum versus initial estimate of outgoing momentum
   double precision, allocatable :: alpha_ene_1D(:) !< [-] ratio of incoming energy versus initial estimate of outgoing energy
   double precision, allocatable :: cfli(:) !< sum of incoming courants (    ) = sum( Dt*Qj/Vi)
   double precision, allocatable :: dvxc(:) !< cell center stress term, global x-dir (m3/s2)
   double precision, allocatable :: dvyc(:) !< cell center stress term, global y-dir (m3/s2)
   double precision, allocatable :: squ(:) !< cell center outgoing flux (m3/s)
   double precision, allocatable :: sqi(:) !< cell center incoming flux (m3/s)
   double precision, allocatable :: squ2D(:) !< cell center outgoing 2D flux (m3/s)
   double precision, allocatable :: sqwave(:) !< cell center outgoing flux, including gravity wave velocity (m3/s) (for explicit time-step)
   double precision, allocatable :: squcor(:) !< cell center outgoing flux with some corrections to exclude structure links (if enabled)
   double precision, allocatable :: hus(:) !< hu averaged at 3D cell
   double precision, allocatable :: workx(:) !< Work array
   double precision, allocatable :: worky(:) !< Work array
   double precision, allocatable :: work0(:, :) !< Work array
   double precision, allocatable :: work1(:, :) !< Work array
   double precision, allocatable, target :: ucx_mor(:) !< [m/s] cell center velocity for sedmor, global x-dir (m/s) {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable, target :: ucy_mor(:) !< [m/s] cell center velocity for sedmor, global y-dir (m/s) {"location": "face", "shape": ["ndkx"]}

   double precision, allocatable :: dsadx(:) !< cell center sa gradient, (ppt/m)
   double precision, allocatable :: dsady(:) !< cell center sa gradient, (ppt/m)

! node related, dim = ndxi
   double precision, allocatable, target :: freeboard(:) !< [m] For output purposes: freeboard at cell center, only for 1D
   double precision, allocatable, target :: hsOnGround(:) !< [m] For output purposes: waterdepth above ground level, only for 1D
   double precision, allocatable, target :: volOnGround(:) !< [m3] For output purposes: volume above ground level, only for 1D
   double precision, allocatable :: qCur1d2d(:) !< [m3/s] total 1d2d net inflow, current discharge
   double precision, allocatable :: vTot1d2d(:) !< [m3] total 1d2d net inflow, cumulative volume
   double precision, allocatable :: qCurLat(:) !< [m3/s] total lateral net inflow, current discharge
   double precision, allocatable :: vTotLat(:) !< [m3] total lateral net inflow, cumulative volume

   ! link related, dim = lnx
   double precision, allocatable :: s1Gradient(:) !< [1] For output purposes: water level gradient on flow links

!    Secondary Flow
   double precision, allocatable :: ducxdx(:) !< cell center gradient of x-velocity in x-dir,    (1/s)
   double precision, allocatable :: ducxdy(:) !< cell center gradient of x-velocity in y-dir,    (1/s)
   double precision, allocatable :: ducydx(:) !< cell center gradient of y-velocity in x-dir,    (1/s)
   double precision, allocatable :: ducydy(:) !< cell center gradient of y-velocity in y-dir,    (1/s)
! double precision, allocatable, target     :: dsdx   (:)   !< cell center gradient of waterlevel in x-dir,    ( )
! double precision, allocatable, target     :: dsdy   (:)   !< cell center gradient of waterlevel in y-dir,    ( )
! double precision, allocatable, target     :: dvdx   (:)   !< cell center gradient of y-velocity in x-dir,    (1/s)
! double precision, allocatable, target     :: dvdy   (:)   !< cell center gradient of y-velocity in y-dir,    (1/s)
! double precision, allocatable, target     :: rsi    (:)   !< 1/R_s inverse streamline curvature         ,    (1/m)
! double precision, allocatable, target     :: rsiexact(:)   !< 1/R_s inverse streamline curvature (exact) ,    (1/m)
! double precision, allocatable, target     :: uc3rsi (:)   !< cell center u_mod^3/R_s                    ,    (m^2/s^3)
   double precision, dimension(:), allocatable :: spircrv !< 1/R_s streamline curvature                 ,    (1/m)
   double precision, dimension(:), allocatable :: spirint !< spiral flow intensity                      ,    (m/s)
   double precision, dimension(:), allocatable :: spirsrc !< source term for spiral flow intensity      ,    (m/s^2)
   double precision, dimension(:), allocatable :: spirfx !< Secondary flow force for momentum in x-dir ,    (m/s^2)
   double precision, dimension(:), allocatable :: spirfy !< Secondary flow force for momentum in y-dir ,    (m/s^2)
   double precision, dimension(:), allocatable :: spirucm !< velocity in the flow node                  ,    (m/s)
   double precision, dimension(:), allocatable :: ht_xx !< array hT_xx, for calculation of spirfx and spirfy
   double precision, dimension(:), allocatable :: ht_xy !< array hT_xy, for calculation of spirfx and spirfy
   double precision, dimension(:), allocatable :: czusf !< Chezy coefficient on flow link
   double precision, dimension(:), allocatable :: czssf !< Chezy coefficient in flow node
   double precision, dimension(:), allocatable :: fcoris !< Coriolis force in the flow node

   double precision, dimension(:), allocatable :: spiratx !< x component of normalised vector in direction of depth averaged velocity    (-)
   double precision, dimension(:), allocatable :: spiraty !< y component of normalised vector in direction of depth averaged velocity    (-)

   double precision :: spirE = 0d0 !< factor for weighing the effect of the spiral flow intensity on transport angle, Eq 11.45 of Delft3D manual
   double precision :: spirbeta = 0d0 !< factor for weighing the effect of the spiral flow on flow dispersion stresses, Eq 9.155 of Delft3D manual
   integer :: numoptsf

! Anti-creep
   double precision, dimension(:), allocatable :: dsalL ! the flux of salinity    on flow linkes for anti-creep
   double precision, dimension(:), allocatable :: dtemL ! the flux of temperature on flow nodes  for anti-creep

   double precision, allocatable, target :: sa0(:) !< [1e-3] salinity (ppt) at start of timestep {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable, target :: sa1(:) !< [1e-3] salinity (ppt) at end   of timestep {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable, target :: satop(:) !< [1e-3] salinity (ppt) help in initialise , deallocated {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: sabot(:) !< [1e-3] salinity (ppt) help in initialise , deallocated {"location": "face", "shape": ["ndx"]}
   double precision, allocatable :: supq(:) !< summed upwind salinity fluxes (ppt*m3/s)
   double precision, allocatable :: qsho(:) !< higher order part of upwind salinity    fluxes (ppt*m3/s) (dim=lnkx)
   double precision, allocatable, target :: tem0(:) !< [degC] water temperature at end of timestep {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable, target :: tem1(:) !< [degC] water temperature at end of timestep {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable :: qtho(:) !< higher order part of upwind temperature fluxes (ppt*m3/s) (dim=lnkx)

   double precision, allocatable :: sam0(:) !< salinity mass       (pptm3) at start of timestep  ! remove later
   double precision, allocatable :: sam1(:) !< salinity mass       (pptm3) at end   of timestep  ! remove later
   double precision, allocatable :: same(:) !< salinity mass error (pptm3) at end   of timestep  ! remove later

   double precision, allocatable :: ww1(:) !< vertical velocity (m/s) end of timestep
   double precision, allocatable :: qw(:) !< vertical flux through interface (m3/s)
   double precision, allocatable :: tidep(:, :) !< tidal potential (m2/s2)
   double precision, allocatable :: tidef(:) !< tidal force (m/s2)
   double precision, allocatable :: s1init(:) !< initial water level, for correction in SAL

   double precision, allocatable :: vih(:) !< horizontal eddy viscosity in cell center (m2/s)
   double precision, allocatable :: qin(:) !< rain, evap, qlat and src netto inloop (m3/s)

   double precision :: errmas !< (cumulative) mass   error ()

! link related, dim = lnkx
   double precision, allocatable, target :: u0(:) !< flow velocity (m/s)  at start of timestep
   double precision, allocatable, target :: u1(:) !< [m/s]  flow velocity (m/s)  at   end of timestep {"location": "edge", "shape": ["lnkx"]}
   double precision, allocatable, target :: u_to_umain(:) !< [-]  Factor for translating general velocity to the flow velocity in the main channel at end of timestep (1d) {"location": "edge", "shape": ["lnkx"]}
   double precision, allocatable, target :: q1(:) !< [m3/s] discharge     (m3/s) at   end of timestep n, used as q0 in timestep n+1, statement q0 = q1 is out of code, saves 1 array {"location": "edge", "shape": ["lnkx"]}
   double precision, allocatable, target :: q1_main(:) !< [m3/s] discharge     (m3/s) in main channel at {"location": "edge", "shape": ["lnkx"]}
   double precision, allocatable :: qa(:) !< discharge (m3/s) used in advection, qa=au(n)*u1(n+1) instead of
   double precision, allocatable :: map_fixed_weir_energy_loss(:) !< fixed weir energy loss at end of timestep {"location": "edge", "shape": ["lnkx"]}
   double precision, allocatable :: cflj(:) !< courant nr link j to downwind volume i (    ) = Dt*Qj/Vi
   double precision, allocatable :: tetaj(:) !< 1-1/sum(upwind incoming courants)      (    )
   double precision, allocatable, target :: au(:) !< [m2] flow area     (m2)   at u point {"location": "edge", "shape": ["lnkx"]}
   double precision, allocatable, target :: au_nostrucs(:) !< [m2] flow area     (m2)   at u point {"location": "edge", "shape": ["lnkx"]}
   double precision, allocatable :: ucxu(:) !< upwind link ucx (m/s)
   double precision, allocatable :: ucyu(:) !< upwind link ucy (m/s)
   double precision, allocatable :: au1D(:, :) !< [m2] cross-sectional area at begin and end of 1D link (only relevant for Pure1D)
   double precision, allocatable :: wu1D(:, :) !< [m] surface width at begin and end of 1D link (only relevant for Pure1D)
   double precision, allocatable :: sar1D(:, :) !< [m2] surface area of first and second half of 1D link (only relevant for Pure1D)
   double precision, allocatable :: volu1D(:) !< [m3] volume of 1D link (only relevant for Pure1D)
   double precision, allocatable :: u1Du(:) !< [m/s] upwind 1D link velocity (only relevant for Pure1D)
   double precision, allocatable :: q1D(:, :) !< [m3/s] discharge at begin and end of 1D link (only relevant for Pure1D)
   integer, allocatable :: isnbnod(:, :) !< sign of left/right node follows your dir in jaPure1D assumptions, -1 or 1 for Ja1D nodes
   integer, allocatable :: isnblin(:, :) !< sign of left/right link follows your dir in jaPure1D assumptions, -1 or 1 for Ja1D nodes
   double precision, allocatable :: advi(:) !< advection implicit part (1/s)
   double precision, allocatable :: adve(:) !< advection explicit part (m/s2)
   double precision, allocatable :: adve0(:) !< advection explicit part (m/s2) prevstep
   double precision, allocatable, target :: hu(:) !< [m] upwind waterheight at u-point; for 3D layers the distance from the top of layer to the bed (m) {"location": "edge", "shape": ["lnx"]}
   double precision, allocatable :: huvli(:) !< inverse alfa weighted waterheight at u-point (m) (volume representative)
   double precision, allocatable :: v(:) !< tangential velocity in u point (m/s)
   double precision, allocatable :: suu(:) !< stress u dir (m/s2)
   double precision, allocatable :: cfuhi(:) !< g/(hCC) u point (1/m)
   double precision, allocatable, target :: frcu(:) !< [TODO] friction coefficient set by initial fields {"location": "edge", "shape": ["lnx"]}
   double precision, allocatable :: frcu_mor(:) !< friction coefficient in morphologically active region set by initial fields {"location": "edge", "shape": ["lnx"]}
   double precision, allocatable :: frcu_bkp(:) !< Backup of friction coefficient set by initial fields {"location": "edge", "shape": ["lnx"]}
   double precision, allocatable :: cfclval(:) !< array for calibration factor for friction coefficients
   double precision, allocatable :: cftrt(:, :) !< array for friction coefficients due to trachytopes
   double precision, allocatable, target :: cftrtfac(:) !< array for optional multiplication factor for trachytopes's returned roughness values
   integer :: jacftrtfac !< Whether or not (1/0) a multiplication factor field was specified for trachytopes's Chezy roughness values.
   double precision, allocatable :: czu(:) !< array for chezy friction at flow links {"location": "edge", "shape": ["lnx"]}
   double precision, allocatable, target :: frculin(:) !< friction coefficient set by initial fields ( todo mag later ook single real worden)
   integer, allocatable :: ifrcutp(:) !< friction coefficient type   initial fields ( todo mag later ook single real worden)
   double precision, allocatable, target :: Cdwusp(:) !< Wind friction coefficient at u point set by initial fields ( todo mag later ook single real worden)
   double precision, allocatable :: wind_speed_factor(:) !< wind speed multiplication factor
   double precision, allocatable :: solar_radiation_factor(:) !< solar radiation multiplication factor
   double precision, allocatable :: z0ucur(:) !< current related roughness, moved from waves, always needed
   double precision, allocatable :: z0urou(:) !< current and wave related roughness

   double precision, allocatable :: frcuroofs(:) !< temp

   double precision, allocatable, target :: frcInternalTides2D(:) !< internal tides friction coefficient gamma, tau/rho = - gamma u.grad h grad h

   double precision, allocatable :: wavfu(:) !< wave force u point
   double precision, allocatable :: wavfv(:) !< wave force u point
   double precision, allocatable :: wdsu(:) !< windstress/rhow u point  (m2/s2)
   double precision, allocatable, target :: wdsu_x(:) !< windstress u point  (N/m2) x-component
   double precision, allocatable, target :: wdsu_y(:) !< windstress u point  (N/m2) y-component
   double precision, allocatable :: wavmubnd(:) !< wave-induced mass flux (on open boundaries)
   integer :: number_steps_limited_visc_flux_links = 0 !< number of steps with limited viscosity/flux on links
   integer, parameter :: MAX_PRINTS_LIMITED_VISC_FLUX_LINKS = 10 !< number of messages in dia file on limited viscosity/flux links
   real(kind=sp), allocatable :: vicLu(:) !< horizontal eddy viscosity coefficient at u point (m2/s)  (limited only if ja_timestep_auto_visc==0)
   real(kind=sp), allocatable :: viu(:) !< horizontal eddy viscosity coefficient at u point (m2/s), modeled part of viscosity = vicLu - viusp
   double precision, allocatable, target :: viusp(:) !< [m2/s] user defined spatial eddy viscosity coefficient at u point (m2/s) {"location": "edge", "shape": ["lnx"]}
   double precision, allocatable, target :: diusp(:) !< [m2/s] user defined spatial eddy diffusivity coefficient at u point (m2/s) {"location": "edge", "shape": ["lnx"]}
   !< so in transport, total diffusivity = viu*sigdifi + diusp
   real, allocatable :: fcori(:) !< spatially variable fcorio coeff at u point (1/s)
   double precision, allocatable :: fvcoro(:) !< 3D adamsbashford u point (m/s2)

   double precision, allocatable :: plotlin(:) !< for plotting on u points
   integer, allocatable :: numlimdt(:) !< nr of times this point was the timestep limiting point
   integer :: numlimdt_baorg = 0 !< nr of times limiting > numlimdt_baorg, keep org ba
   double precision :: baorgfracmin = 0 !< ba = max(cutarea, ba*baorgfracmin)

   double precision, allocatable :: zn2rn(:) !< weight from zn to rn, flownode to netnode

   double precision, allocatable, target :: tausx(:) ! vector components shear stress
   double precision, allocatable, target :: tausy(:)
   double precision, allocatable, target :: taubxu(:) !< Maximal bed shear stress
   double precision, allocatable, target :: taubu(:) !< Mean bed shear stress
   double precision, allocatable :: q1waq(:) !< Cumulative q1 within current waq-timestep
   double precision, allocatable :: qwwaq(:) !< Cumulative qw within current waq-timestep

   ! solving related, dim = ndx for 2D, otherwise ndx*kmxd
   double precision, allocatable :: fu(:) !< main diag (lnx)
   double precision, allocatable :: ru(:) !< rhs       (lnx)
   double precision, allocatable :: bb(:) !< main diag (ndx)
   double precision, allocatable :: dd(:) !< rhs       (ndx)

   integer, allocatable :: struclink(:)

   ! basis
   double precision :: vol0tot !< Total volume start of timestep            (m3)
   double precision :: vol1tot !< Total volume   end of timestep            (m3)
   double precision :: vol1ini !< Total volume   initially                  (m3)
   double precision :: vol1icept !< Total volume interception end of timestep (m3)
   double precision :: Volgrw !< Total volume grw end of timestep          (m3)
   double precision :: Volgrwini !< Total volume grw initially                (m3)

   double precision :: qinbnd !< Actual influx boundaries                  (m3/s)
   double precision :: qoutbnd !< Actual outflux boundaries                 (m3/s)
   double precision :: qincel !< Actual influx cells                       (m3/s)
   double precision :: qoutcel !< Actual outflux cells                      (m3/s)

   double precision :: vinbnd !< Volume in  boundaries of timestep         (m3)
   double precision :: voutbnd !< Volume out boundaries of timestep         (m3)
   double precision :: vincel !< Volume in  cells      of timestep         (m3)
   double precision :: voutcel !< Volume out cells      of timestep         (m3)
   double precision :: volerr !< Volume error of timestep vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel   (m3)

   double precision :: vinbndcum !< Cumulative volume through boundaries in   (m3) Cumulative values
   double precision :: voutbndcum !< Cumulative volume through boundaries out  (m3)
   double precision :: vincelcum !< Cumulative volume in  cells               (m3/s) Actual values
   double precision :: voutcelcum !< Cumulative volume out cells               (m3/s)
   double precision :: volerrcum !< Volume error since start of computation   (m3)

   double precision :: dvolbot !<     (m3), associated with jamorf

   ! extra
   double precision :: qinrain !< Total influx rain                         (m3/s)
   double precision :: qinrainground !< Total influx rain onto the ground       (m3/s)
   double precision :: qouteva !< Total outflux evaporation                 (m3/s)
   double precision :: qoutevaicept !< Total outflux evaporation from interception layer (m3/s)
   double precision, dimension(2) :: qinlat !< Total influx diffuse laterals (1D and 2D) (m3/s)
   double precision, dimension(2) :: qoutlat !< Total outflux diffuse laterals (1D and 2D)(m3/s)
   double precision :: qingrw !< Total influx groundwater                  (m3/s)
   double precision :: qoutgrw !< Total outflux groundwater                 (m3/s)
   double precision :: qinsrc !< Total influx local point sources          (m3/s)
   double precision :: qoutsrc !< Total outflux local pount sources         (m3/s)
   double precision, dimension(2) :: qinext !< Total influx Qext (1D and 2D)             (m3/s)
   double precision, dimension(2) :: qoutext !< Total outflux Qext(1D and 2D)             (m3/s)

   double precision :: vinrain !< Total volume in  rain                     (m3) in the last time step
   double precision :: vinrainground !< Total volume of rain falling onto the ground (in the last time step) (m3)
   double precision :: vouteva !< Total volume out evaporation              (m3)
   double precision :: voutevaicept !< Total volume out evaporation from interception layer (m3)
   double precision, dimension(2) :: vinlat !< Total volume in  diffuse laterals (1D and 2D) (m3)
   double precision, dimension(2) :: voutlat !< Total volume out diffuse laterals (1D and 2D) (m3)
   double precision :: vingrw !< Total volume in  groundwater              (m3)
   double precision :: voutgrw !< Total volume out groundwater              (m3)
   double precision :: vinsrc !< Total volume in  local point sources      (m3)
   double precision :: voutsrc !< Total volume out local pount sources      (m3)
   double precision, dimension(2) :: vinext !< Total volume in  Qext (1D and 2D)         (m3)
   double precision, dimension(2) :: voutext !< Total volume out Qext (1D and 2D)         (m3)

   double precision :: vinraincum !< Total inflow from rain                    (m3) integrated over all time steps
   double precision :: voutevacum !< Total outflow to evaporation              (m3) "
   double precision, dimension(2) :: vinlatcum !< Total inflow from diffuse laterals (1D and 2D) (m3) "
   double precision, dimension(2) :: voutlatcum !< Total outflow to diffuse laterals  (1D and 2D) (m3) "
   double precision :: vingrwcum !< Total inflow from groundwater             (m3) "
   double precision :: voutgrwcum !< Total outflow to groundwater              (m3) "
   double precision :: vinsrccum !< Total inflow from local point sources     (m3) "
   double precision :: voutsrccum !< Total outflow to local pount sources      (m3) "
   double precision, dimension(2) :: vinextcum !< Total inflow from Qext (1D and 2D)        (m3) "
   double precision, dimension(2) :: voutextcum !< Total outflow to  Qext (1D and 2D)        (m3) "

   double precision :: DissInternalTides !< Total Internal Tides Dissipation (J/s)
   double precision, allocatable :: DissInternalTidesPerArea(:) !< Internal tides dissipation / area (J/(m^2 s))
   double precision :: GravInput !< Total Gravitational Input (incl. SAL) (J/s)
   double precision :: SALInput !< Total SAL Input (J/s)
   double precision :: SALInput2 !< Total SAL Input (J/s), different formulation

   double precision :: a0tot !< Total wet surface area start of timestep (m2)
   double precision :: a1tot !< Total wet surface area   end of timestep (m2)
   double precision :: a1ini !< Total model area rain evap               (m2)
   double precision :: ek1tot !< Volume averaged kin energy (m2/s2) end of timestep
   double precision :: ep1tot !< Volume averaged pot energy (m2/s2) end of timestep
   double precision :: ep1rela !< Time av ep1tot
   double precision :: hsaver !< Average waterdepth (m), vol/are

   ! basis zout
   double precision :: sam0tot !< Total mass start of timestep            (m3ppt)
   double precision :: sam1tot !< Total mass   end of timestep            (m3ppt)
   double precision :: sam1ini = -1d0 !< Total mass initially                    (m3ppt)

   double precision :: saminbnd !< Actual mass in  boundaries of timestep  (m3ppt)
   double precision :: samoutbnd !< Actual mass out boundaries of timestep  (m3ppt)
   double precision :: samerr !< vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel   (m3)

   double precision :: saminbndcum !< Cumulative mass in  boundaries          (m3ppt) Cumulative values
   double precision :: samoutbndcum !< Cumulative mass out boundaries          (m3ppt)
   double precision :: samerrcum !< Mass error since start of computation   (m3ppt)

   double precision :: epsmaxvol !< eps vol diff (m3) ! both not used now
   double precision :: difmaxlev !< max lev diff (m)
   double precision :: epsmaxlev = 1d-8 !< eps lev diff (m)
   double precision :: epsmaxlevm = 1d-8 !< eps lev diff (m) minus part

   logical :: debugon !< texts  yes or no
   logical :: validateon !< should we validate flow state yes or no (switched off at water drop)
   integer :: noddifmaxlev !< node number of max lev diff ()
   integer :: nodneg !< node nr with negative hs
   integer :: numnodneg !< nr of posh checks
   integer :: jaLinkdried !< there was at least 1 setback in this step
   integer :: Linkdriedmx = 0 !< max nr of au growth steps after having dried
   integer :: nodnegtek !< node nr with negative hs to draw
   integer :: kkcflmx !< 2D Node nr with max courant
   integer :: kcflmx !< 3D Node nr with max courant
   integer :: itsol !< act nr. of iterations in solve
   integer :: nochkadv !< nr of chkadvd checks
   integer :: nrimptran !< nr of implicit transport points
   integer :: ndmin !< node nr where min znod is found in viewing area
   integer :: ndmax !< node nr where max znod is found in viewing area
   integer :: Lnmin !< link nr where min zlin is found in viewing area
   integer :: Lnmax !< link nr where max zlin is found in viewing area

   integer, parameter :: MAX_IDX = 40
   double precision, dimension(MAX_IDX) :: volcur !< Volume totals in *current* timestep only (only needed for MPI reduction)
   double precision, dimension(MAX_IDX) :: cumvolcur = 0d0 !< Cumulative volume totals starting from the previous His output time, cumulate with volcur (only needed for MPI reduction)
   double precision, dimension(MAX_IDX), target :: voltot
   character(len=100), dimension(MAX_IDX) :: voltotname
   integer, parameter :: IDX_VOLTOT = 1
   integer, parameter :: IDX_STOR = 2
   integer, parameter :: IDX_VOLERR = 3
   integer, parameter :: IDX_BNDIN = 4
   integer, parameter :: IDX_BNDOUT = 5
   integer, parameter :: IDX_BNDTOT = 6
   integer, parameter :: IDX_EXCHIN = 7
   integer, parameter :: IDX_EXCHOUT = 8
   integer, parameter :: IDX_EXCHTOT = 9
   integer, parameter :: IDX_PRECIP_TOTAL = 10
   integer, parameter :: IDX_EVAP = 11
   integer, parameter :: IDX_SOUR = 12
   integer, parameter :: IDX_InternalTidesDissipation = 13
   integer, parameter :: IDX_GravInput = 14
   integer, parameter :: IDX_SALInput = 15
   integer, parameter :: IDX_SALInput2 = 16
   integer, parameter :: IDX_GRWIN = 17
   integer, parameter :: IDX_GRWOUT = 18
   integer, parameter :: IDX_GRWTOT = 19
   integer, parameter :: IDX_LATIN = 20
   integer, parameter :: IDX_LATOUT = 21
   integer, parameter :: IDX_LATTOT = 22
   integer, parameter :: IDX_LATIN1D = 23
   integer, parameter :: IDX_LATOUT1D = 24
   integer, parameter :: IDX_LATTOT1D = 25
   integer, parameter :: IDX_LATIN2D = 26
   integer, parameter :: IDX_LATOUT2D = 27
   integer, parameter :: IDX_LATTOT2D = 28
   integer, parameter :: IDX_EXTIN = 29
   integer, parameter :: IDX_EXTOUT = 30
   integer, parameter :: IDX_EXTTOT = 31
   integer, parameter :: IDX_EXTIN1D = 32
   integer, parameter :: IDX_EXTOUT1D = 33
   integer, parameter :: IDX_EXTTOT1D = 34
   integer, parameter :: IDX_EXTIN2D = 35
   integer, parameter :: IDX_EXTOUT2D = 36
   integer, parameter :: IDX_EXTTOT2D = 37
   integer, parameter :: IDX_ICEPT = 38
   integer, parameter :: IDX_EVAP_ICEPT = 39
   integer, parameter :: IDX_PRECIP_GROUND = 40

   logical :: ucxyq_read_rst !< determines if variables `ucxq` and `ucxy` have been read from restart.
   logical :: rho_read_rst !< determines if variable  `rho`             has  been read from restart.

contains
!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_flow() instead.
   subroutine default_flow()
! 3D parameters
      ! kmx         = 0    ! nr of 3d layers, increasing in positive upward direction
      ! if kmx==0 then 2D code. if kmx==1 then 3D code
      ! kmx1        = kmx+1 ! kmx + 1, for dimensioning arrays that used be (0:kmax)
      ! kmxd        = 0    ! dim of kmx, >= 1
      ! ndkx        = 0    ! dim of 3d flow nodes (internal + boundary)
      ! ndkx1       = 1    ! dim of 3d flow horizontal interfaces (internal + boundary), (0:kmx)
      ! lnkx        = 0    ! dim of 3d flow links (internal + boundary)

      mxlayz = 1 ! max nr of z     layers in flow domain
      mxlays = 1 ! max nr of sigma layers in flow domain
      kplot = 1 ! layer nr to be plotted
      nplot = 1 ! vertical profile to be plotted at node nr
      layertype = 1 !< 1= all sigma, 2 = all z, 3 = left sigma, 4 = left z
      iturbulencemodel = 3 !< 0=no, 1 = constant, 2 = algebraic, 3 = k-eps, 4 = k-tau
      ieps = 2 !< bottom boundary type eps. eqation, 1=dpmorg, 2 = dpmsandpit, 3=D3D, 4=Dirichlethdzb
      sigmagrowthfactor = 1d0 !<layer thickness growth factor from bed up

      ! Remaining of variables is handled in reset_flow()
      call reset_flow()
   end subroutine default_flow

!> Resets only flow variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_flow() instead.
   subroutine reset_flow()
      use m_missing
! node related

! basis
      vol0tot = 0 ! total volume start of timestep          (m3)
      vol1tot = 0 ! total volume   end of timestep          (m3)
      vol1icept = 0d0 ! total volume interception end of timestep (m3)
      vol1ini = -1d0 ! total volume   initially                (m3)
      Volgrw = 0 ! total grw volume                        (m3)
      Volgrwini = 0d0 ! total grw volume initially              (m3)

      qinbnd = 0 ! total inflow boundaries                 (m3/s) Actual values
      qoutbnd = 0 ! total outflow boundaries                (m3/s)
      qincel = 0 ! total inflow cells                      (m3/s) Actual values
      qoutcel = 0 ! total outflow cells                     (m3/s)

      vinbnd = 0 ! total volume in  boundaries             (m3) Actual values
      voutbnd = 0 ! total volume out boundaries             (m3)
      vincel = 0 ! total volume in  cells                  (m3) Actual values
      voutcel = 0 ! total volume out cells                  (m3)
      volerr = 0 ! vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel   (m3)

      vinbndcum = 0 ! total inflow boundaries                 (m3) Cumulative values
      voutbndcum = 0 ! total outflow boundaries                (m3)
      vincelcum = 0 ! total volume in  cells                  (m3) Actual values
      voutcelcum = 0 ! total volume out cells                  (m3)
      volerrcum = 0 !     (m3)

      dvolbot = 0d0 !     (m3)

      ! extra
      qinrain = 0 ! total inflow rain                       (m3/s)
      qinrainground = 0 ! Total influx rain onto the ground       (m3/s)
      qouteva = 0 ! total outflow evaporation               (m3/s)
      qoutevaicept = 0 ! JanM ??? [?]
      qinlat(1:2) = 0 ! total inflow diffuse laterals           (m3/s)
      qoutlat(1:2) = 0 ! total outflow diffuse laterals          (m3/s)
      qingrw = 0 ! total inflow from groundwater           (m3/s)
      qoutgrw = 0 ! total outflow to groundwater            (m3/s)
      qinsrc = 0 ! total inflow local point sources        (m3/s)
      qoutsrc = 0 ! total outflow local pount sources       (m3/s)
      qinext(1:2) = 0 ! total influx Qext (1D and 2D)           (m3/s)
      qoutext(1:2) = 0 ! total outflux Qext(1D and 2D)           (m3/s)

      vinrain = 0 ! total volume in  rain                   (m3)
      vinrainground = 0 ! Total volume of rain falling onto the ground (in the last time step) (m3)
      vouteva = 0 ! total volume out evaporation            (m3)
      voutevaicept = 0 ! total volume out evaporation from interception layer (m3)
      vinlat(1:2) = 0 ! total volume in diffuse laterals       (m3)
      voutlat(1:2) = 0 ! total volume out diffuse laterals       (m3)
      vingrw = 0 ! total volume in  groundwater            (m3)
      voutgrw = 0 ! total volume out groundwater            (m3)
      vinsrc = 0 ! total volume in  local point sources    (m3)
      voutsrc = 0 ! total volume out local pount sources    (m3)
      vinext(1:2) = 0 ! total volume in  Qext (1D and 2D)       (m3)
      voutext(1:2) = 0 ! total volume out Qext (1D and 2D)       (m3)

      vinraincum = 0 ! total inflow rain                       (m3)
      voutevacum = 0 ! total outflow evaporation               (m3)
      vinlatcum(1:2) = 0 ! total inflow diffuse laterals           (m3)
      voutlatcum(1:2) = 0 ! total outflow diffuse laterals          (m3)
      vingrwcum = 0 ! total inflow groundwater                (m3)
      voutgrwcum = 0 ! total outflow groundwater               (m3)
      vinsrccum = 0 ! total inflow local point sources        (m3)
      voutsrccum = 0 ! total outflow local pount sources       (m3)
      vinextcum(1:2) = 0 ! total inflow from Qext (1D and 2D)      (m3)
      voutextcum(1:2) = 0 ! total outflow to  Qext (1D and 2D)      (m3)

      DissInternalTides = 0d0 !< total Internal Tides Dissipation (J/s)

      a0tot = 0 ! total wet surface area start of timestep (m2)
      a1tot = 0 ! total wet surface area   end of timestep (m2)
      a1ini = 0 ! total model       area                   (m2)
      ek1tot = 0 ! volume averaged kin energy (m2/s2) end of timestep
      ep1tot = 0 ! volume averaged pot energy (m2/s2) end of timestep
      ep1rela = 0 ! time relaxe ep1tot
      hsaver = 0 ! average waterdepth (m), vol/are

      epsmaxvol = 1d-9 ! eps vol diff (m3) ! both not used now
      difmaxlev = 0 ! max lev diff (m3)
      !epsmaxlev   = 1d-8 ! eps lev diff (m)  ! max waterlevel difference in Newton iterations
      !epsmaxlevm  = 1d-8 ! eps lev diff (m)  ! max waterlevel difference in Newton iterations

      debugon = .false. ! texts  yes or no
      validateon = .true. ! validate flow state yes or no

      nodneg = 0 ! node nr with negative hs
      kkcflmx = 0 ! 2D node nr with max courant
      itsol = 0 ! act nr. of iterations in solve
      nochkadv = 0 ! nr of chkadvd checks
      numnodneg = 0 ! nr of posh checks
      nrimptran = 0 ! nr of implicit transport points
      ndmin = 0 ! node nr where min znod is found in viewing area
      ndmax = 0 ! node nr where max znod is found in viewing area
      Lnmin = 0 ! link nr where min zlin is found in viewing area
      Lnmax = 0 ! link nr where max zlin is found in viewing area

      sam0tot = 0d0 !< Total mass start of timestep            (m3ppt)
      sam1tot = 0d0 !< Total mass   end of timestep            (m3ppt)
      samerr = 0d0 !< vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel   (m3)

      voltot(:) = 0d0
      voltotname(IDX_VOLTOT) = 'total_volume'
      voltotname(IDX_STOR) = 'storage'
      voltotname(IDX_VOLERR) = 'volume_error'
      voltotname(IDX_BNDIN) = 'boundaries_in'
      voltotname(IDX_BNDOUT) = 'boundaries_out'
      voltotname(IDX_BNDTOT) = 'boundaries_total'
      voltotname(IDX_EXCHIN) = 'exchange_with_1D_in'
      voltotname(IDX_EXCHOUT) = 'exchange_with_1D_out'
      voltotname(IDX_EXCHTOT) = 'exchange_with_1D_total'
      voltotname(IDX_PRECIP_TOTAL) = 'precipitation_total'
      voltotname(IDX_EVAP) = 'evaporation'
      voltotname(IDX_SOUR) = 'source_sink'
      voltotname(IDX_InternalTidesDissipation) = 'InternalTidesDissipation'
      voltotname(IDX_GravInput) = 'Gravitational_Input'
      voltotname(IDX_SalInput) = 'SAL_Input'
      voltotname(IDX_SalInput2) = 'SAL_Input_2'
      voltotname(IDX_GRWIN) = 'groundwater_in'
      voltotname(IDX_GRWOUT) = 'groundwater_out'
      voltotname(IDX_GRWTOT) = 'groundwater_total'
      voltotname(IDX_LATIN) = 'laterals_in'
      voltotname(IDX_LATOUT) = 'laterals_out'
      voltotname(IDX_LATTOT) = 'laterals_total'
      voltotname(IDX_LATIN1D) = 'laterals_in_1D'
      voltotname(IDX_LATOUT1D) = 'laterals_out_1D'
      voltotname(IDX_LATTOT1D) = 'laterals_total_1D'
      voltotname(IDX_LATIN2D) = 'laterals_in_2D'
      voltotname(IDX_LATOUT2D) = 'laterals_out_2D'
      voltotname(IDX_LATTOT2D) = 'laterals_total_2D'
      voltotname(IDX_EXTIN) = 'Qext_in'
      voltotname(IDX_EXTOUT) = 'Qext_out'
      voltotname(IDX_EXTTOT) = 'Qext_total'
      voltotname(IDX_EXTIN1D) = 'Qext_in_1D'
      voltotname(IDX_EXTOUT1D) = 'Qext_out_1D'
      voltotname(IDX_EXTTOT1D) = 'Qext_total_1D'
      voltotname(IDX_EXTIN2D) = 'Qext_in_2D'
      voltotname(IDX_EXTOUT2D) = 'Qext_out_2D'
      voltotname(IDX_EXTTOT2D) = 'Qext_total_2D'
      voltotname(IDX_ICEPT) = 'total_volume_interception'
      voltotname(IDX_EVAP_ICEPT) = 'evaporation_interception'
      voltotname(IDX_PRECIP_GROUND) = 'precipitation_on_ground'

      jacftrtfac = 0 !< Whether or not (1/0) a multiplication factor field was specified for trachytopes's returned roughness values.

      upot0 = dmiss
      ukin0 = dmiss

   end subroutine reset_flow

end module m_flow
