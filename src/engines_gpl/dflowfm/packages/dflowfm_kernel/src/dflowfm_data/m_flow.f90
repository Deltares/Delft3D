!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_flow ! flow arrays-999
   use fm_external_forcings_data
   use m_alloc
   use m_density_parameters, only: idensform, apply_thermobaricity, thermobaricity_in_pressure_gradient, &
                                   max_iterations_pressure_density, jabarocponbnd
   use m_flowparameters
   use m_flowoutput
   use m_grw
   use m_heatfluxes
   use m_physcoef
   use m_ship
   use m_turbulence
   use m_vegetation

   implicit none

   ! 3D parameters
   integer :: kmx !< [-] Number of 3d layers, increasing in positive upward direction. 
                  !! When kmx==0 compute as 2D code, when kmx==1 compute as 3D code.
   integer :: kmx1 !< [-] kmx + 1, for dimensioning arrays that used to be (0:kmax).
   integer :: kmxd !< [-] Dim of kmx, >= 1.
   integer :: ndkx !< [-] Dim of 3d flow nodes (internal + boundary).
   integer :: ndkx1 !< [-] Dim of 3d flow horizontal interfaces (internal + boundary), (0:kmx).
   integer :: lnkx !< [-] Dim of 3d flow links (internal + boundary).
   integer :: numvertdis !< [-] Number of vertical layer distributions.
   integer :: mxlayz !< [-] Maximum number of z layers in flow domain.
   integer :: mxlays !< [-] Maximum number of sigma layers in flow domain.
   integer :: kplot !< [-] Layer number to be plotted.
   integer :: nplot !< [-] Vertical profile to be plotted at node number.
   integer :: kplotfrombedorsurface = 2 !< [-] Up or down k.
   integer :: kplotordepthaveraged = 1 !< [-] 1 = kplot, 2 = averaged.
   integer :: numtopsig = 0 !< [-] Number of top layers in sigma.
   integer :: janumtopsiguniform = 1 !< [-] Specified number of top layers in sigma is same everywhere.

   integer, allocatable, dimension(:) :: ndkx_to_ndx !< [-] Maps ndkx to ndx.

   real(kind=dp) :: Tsigma = 100 !< [-] Relaxation period; only used in density controlled sigma-layers (layertype == LAYTP_DENS_SIGMA).

   integer :: layertype !< [-] Vertical layertype, use one of LAYTP_SIGMA, LAYTP_Z, LAYTP_POLYGON_MIXED, LAYTP_DENS_SIGMA parameters.
   integer, parameter :: LAYTP_SIGMA = 1 !< [-] Sigma-layers.
   integer, parameter :: LAYTP_Z = 2 !< [-] Fixed z- or z-sigma-layers.
   integer, parameter :: LAYTP_POLYGON_MIXED = 3 !< [-] Mixed layering in polygon regions (layer count + layertype in each polygon's z-values).
   integer, parameter :: LAYTP_DENS_SIGMA = 4 !< [-] Density controlled sigma-layers.

   integer, parameter :: STRETCH_UNI_OVER_EXP = -1 !< [-] Uniform over exponential, for backward compatibility (only for layertype = LAYTP_Z).
   integer, parameter :: STRETCH_UNIFORM = 0 !< [-] Uniform layers.
   integer, parameter :: STRETCH_USER = 1 !< [-] User defined layers.
   integer, parameter :: STRETCH_EXPONENT = 2 !< [-] Exponential layers.
   integer, parameter :: STRETCH_UNDEFINED = -999 !< [-] Undefined stretching type.
   integer :: stretch_type = STRETCH_UNDEFINED !< [-] Stretching type for layers.

   integer :: iturbulencemodel !< [-] Turbulence model to be used (0 = none, 1 = constant, 2 = algebraic, 3 = k-eps, 4 = k-tau).
   integer, parameter :: TURBULENCE_MODEL_NONE = 0 !< [-] No turbulence model.
   integer, parameter :: TURBULENCE_MODEL_CONSTANT = 1 !< [-] Constant turbulence model.
   integer, parameter :: TURBULENCE_MODEL_ALGEBRAIC = 2 !< [-] Algebraic turbulence model.
   integer, parameter :: TURBULENCE_MODEL_KEPS = 3 !< [-] k-epsilon turbulence model, computed on flow links.
   integer, parameter :: TURBULENCE_MODEL_KTAU = 4 !< [-] k-tau turbulence model.

   integer :: ieps !< [-] Bottom boundary type eps. eqation, 1=dpmorg, 2 = dpmsandpit, 3=D3D, 4=Dirichlethdzb.
   real(kind=dp) :: tur_time_int_factor = 0 !< [-] Turbulence time integration factor for using LAX-based-scheme (0.0 - 1.0) for turbulent quantities (0.0: flow links, 0.5: fifty-fifty, 1.0: flow nodes).
   integer :: tur_time_int_method = TURB_LAX_CONNECTED !< [-] Where to apply tur_time_int_factor (1: apply to all cells, 2: only when vertical layers are horizontally connected).
   real(kind=dp) :: z_layer_growth_factor !< [-] z-layer thickness growth factor from DzTopUniAboveZ downwards.
   real(kind=dp) :: dztopuniabovez = -999.0_dp !< [-] Bottom level of lowest uniform layer == blmin if not specified.
   real(kind=dp) :: Floorlevtoplay = -999.0_dp !< [-] Floor  level of top zlayer, == sini if not specified.
   real(kind=dp) :: dztop = -999.0_dp !< [-] If specified, dz of top layer, kmx = computed, if not, dz = (ztop-zbot)/kmx.
   real(kind=dp) :: zlaybot = -999.0_dp !< [-] If specified, first zlayer starts from zlaybot, if not, it starts from the lowest bed point.
   real(kind=dp) :: zlaytop = -999.0_dp !< [-] If specified, highest zlayer ends at zlaytop, if not, it ends at the initial water level.
   real(kind=dp), allocatable, dimension(:) :: aak !< [-] Coefficient vertical mom exchange of kmx layers.
   real(kind=dp), allocatable, dimension(:) :: bbk !< [-] Coefficient vertical mom exchange of kmx layers.
   real(kind=dp), allocatable, dimension(:) :: cck !< [-] Coefficient vertical mom exchange of kmx layers.
   real(kind=dp), allocatable, dimension(:) :: ddk !< [-] Coefficient vertical mom exchange of kmx layers.
   real(kind=dp), allocatable, dimension(:) :: eek !< [-] Coefficient vertical mom exchange of kmx layers.
   real(kind=dp), allocatable, dimension(:) :: uuk !< [-] Coefficient vertical mom exchange of kmx layers.

   real(kind=dp), allocatable, dimension(:) :: laycof !< [-] Coefficients for sigma layer.
                                                      !! If stretch_type = STRETCH_USER: Percentages of the layers, user defined, laycof(kmx).
                                                      !! If stretch_type = STRETCH_EXPONENT: Stretching level, and two coefficients for layers growth, laycof(3).
   
   real(kind=dp), allocatable, dimension(:, :) :: dzslay !< [-] The normalized thickness of layer, dim = (:,maxlaydefs).

   real(kind=dp), allocatable, target, dimension(:) :: zws !< [m] z-levels of interfaces (w-points) at cell centres. (s-points) (1:ndkx) {"shape": ["ndkx"]}
   real(kind=dp), allocatable, dimension(:) :: zws0 !< [-] z-levels of interfaces (w-points) at cell centres. (s-points) (1:ndkx)

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

   real(kind=dp), allocatable, target, dimension(:) :: zcs !< [-] z-levels at layer mid-points, only for nudging.
   integer, allocatable, target, dimension(:) :: kbot !< [-] Layer-compressed bottom layer cell number: for each of ndx horizontal cells, we have indices to bot and top ndxk cells. {"location": "face", "shape": ["ndx"]}
   integer, allocatable, target, dimension(:) :: ktop !< [-] Layer-compressed top layer cell number: for each of ndx horizontal cells, we have indices to bot and top ndxk cells. {"location": "face", "shape": ["ndx"]}
   integer, allocatable, dimension(:) :: ktop0 !< [-] Storage of ktop.
   integer, allocatable, dimension(:) :: kmxn !< [-] Maximum number of active vertical cells per horizontal base cell n (cell_index_2d). 
                                              !! The maximum is decided upon initialization, depends on many keywords and can be smaller than kmx.
   integer, allocatable, target, dimension(:) :: Lbot !< [-] Layer-compressed bottom layer edge number: for each of lnx horizontal links, we have indices to bot and top lnxk links. {"location": "edge", "shape": ["lnx"]}
   integer, allocatable, target, dimension(:) :: Ltop !< [-] Layer-compressed top layer edge number: for each of lnx horizontal links, we have indices to bot and top lnxk links. {"location": "edge", "shape": ["lnx"]}
   integer, allocatable, dimension(:) :: kmxL !< [-] Maximum number of vertical links per base link L.
   integer, allocatable, dimension(:) :: kbotc !< [-] As kbot, for cornerpoints.
   integer, allocatable, dimension(:) :: kmxc !< [-] As kmxn, for cornerpoints.

   integer :: mxlaydefs = 4 !< [-] Maximum number of layering definitions.
   integer, allocatable, dimension(:) :: laydefnr !< [-] Pointer to laydef, if positive to unique laydef, otherwise interpolate in 1, 2, and 3. {size=ndx}
   integer, allocatable, dimension(:) :: laytyp !< [-] 1 = sigma, 2 = z. {size=mxlaydefs}
   integer, allocatable, dimension(:) :: laymx !< [-] Maximum number of layers. {size=mxlaydefs}
   integer, allocatable, dimension(:) :: nrlayn !< [-] Maximum number of layers. {size=ndx}
   integer, allocatable, dimension(:) :: nlaybn !< [-] Bed layer number. {size=ndx}
   real(kind=dp), allocatable, dimension(:, :) :: zslay !< [-] Z or S coordinate. {size=(:,maxlaydefs)}
   real(kind=dp), allocatable, dimension(:, :) :: wflaynod !< [-] Weight factors to flownodes indlaynod. {size=(3,ndx)}
   integer, allocatable, dimension(:, :) :: indlaynod !< [-] Indices of flownodes. {size=(3,ndx)}
   real(kind=dp), allocatable, dimension(:) :: dkx !< [-] Sigma level of interface height; only used in density controlled sigma-layers (layertype == LAYTP_DENS_SIGMA). {size=ndx}
   real(kind=dp), allocatable, dimension(:) :: sdkx !< [-] Sum of ..; only used in density controlled sigma-layers (layertype == LAYTP_DENS_SIGMA). {size=ndx}

   real(kind=dp), allocatable, dimension(:) :: asig !< [-] Alpha of sigma at nodes, 1d0=full sigma, 0d0=full z, 0.5d0=fifty/fifty; only used in density controlled sigma-layers (layertype == LAYTP_DENS_SIGMA). {size=ndx}
   real(kind=dp), allocatable, dimension(:) :: ustb !< [-] Ustar at Lbot. {size=Lnx}
   real(kind=dp), allocatable, dimension(:) :: ustw !< [-] Ustar at Ltop. {size=Lnx}
   real(kind=dp), allocatable, dimension(:) :: ustbc !< [-] Ustar at bed at netnodes. {size=numk}

   integer :: nfixed, nsigma

   ! flow arrays

   ! node related, dim = ndx
   real(kind=dp), allocatable, target, dimension(:) :: s0 !< [m] Waterlevel at start of timestep. {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target, dimension(:) :: s1 !< [m] Waterlevel at end of timestep. {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target, dimension(:) :: s1max !< [m] Maximum waterlevel at end of timestep for Fourier output. {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, dimension(:) :: s00 !< [m] Waterlevel for checking iteration in nonlinear solver.
   real(kind=dp), allocatable, target, dimension(:) :: a0 !< [m2] Storage area at start of timestep. {"location": "face", "shape": ["ndkx"]}; in 2D models ndkx=ndx
   real(kind=dp), allocatable, target, dimension(:) :: a1 !< [m2] Storage area at end of timestep. {"location": "face", "shape": ["ndkx"]}; in 2D models ndkx=ndx
   real(kind=dp), allocatable, target, dimension(:) :: vol1 !< [m3] total volume at end of timestep. {"location": "face", "shape": ["ndkx"]}; in 2D models ndkx=ndx
   real(kind=dp), allocatable, target, dimension(:) :: vol0 !< [m3] total volume at start of timestep. {"location": "face", "shape": ["ndkx"]}; in 2D models ndkx=ndx
   real(kind=dp), allocatable, target, dimension(:) :: vol1_f !< [m3] Flow volume at end of timestep. {"location": "face", "shape": ["ndkx"]}; in 2D models ndkx=ndx
   real(kind=dp), allocatable, dimension(:) :: sq !< [m3/s] Total influx at water level point.
   real(kind=dp), allocatable, dimension(:) :: sqa !< [m3/s] Total outflux at s point, u1 based, non-conservative for iadvec==38.
   real(kind=dp), allocatable, target, dimension(:) :: hs !< [m] Waterdepth at cell centre = s1 - bl. {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, dimension(:) :: cfs !< [-] Dimensionless friction coefficient sag/C in cell centre.
   real(kind=dp), allocatable, dimension(:) :: volerror !< [-] Volume error.

   real(kind=dp), allocatable, dimension(:) :: voldhu !< node volume based on downwind hu

   real(kind=dp), allocatable, dimension(:) :: s1m !< [m] Waterlevel, pressurized nonlinear minus part.
   real(kind=dp), allocatable, dimension(:) :: a1m !< [m2] Surface area, pressurized nonlinear minus part.

   real(kind=dp), allocatable, dimension(:) :: negativeDepths !< [-] Number of negative depths during output interval at nodes.
   real(kind=dp), allocatable, dimension(:) :: negativeDepths_cum !< [-] Cumulative number of negative depths at nodes.
   real(kind=dp), allocatable, dimension(:) :: noIterations !< [-] Number of no iteration locations during output interval at nodes.
   real(kind=dp), allocatable, dimension(:) :: noIterations_cum !< [-] Cumulative number of no iteration locations at nodes.
   real(kind=dp), allocatable, dimension(:) :: limitingTimestepEstimation !< [-] Number of times during the output interval the conditions in a node is limiting the time step
   real(kind=dp), allocatable, dimension(:) :: limitingTimestepEstimation_cum !< [-] Cumulative number of times the conditions in a node is limiting the time step.
   !< Note: this doubles with variable numlimdt(:), which contains the same cumulative count, under a different MDU option.
   !< Note: these variables are real(kind=dp) (in stead of integers) because post processing is
   !<       based on real(kind=dp) variables.
   real(kind=dp), allocatable, dimension(:) :: flowCourantNumber !< [-] Courant number.

   real(kind=dp), allocatable, dimension(:) :: volau !< [m3] Trial, au based cell volume.
   real(kind=dp), allocatable, target, dimension(:) :: ucx !< [m/s] Cell center velocity, global x-dir. {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: ucy !< [m/s] Cell center velocity, global y-dir. {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: ucz !< [m/s] Cell center velocity, global z-dir. {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: ucxq !< [m/s] Cell center velocity, q based  global x-dir.
   real(kind=dp), allocatable, target, dimension(:) :: ucyq !< [m/s] Cell center velocity, q based  global y-dir.
   real(kind=dp), allocatable, dimension(:) :: uqcx !< [m4/s2] Cell center incoming momentum, global x-dir, only for iadvec = 1.
   real(kind=dp), allocatable, dimension(:) :: uqcy !< [m4/s2] Cell center incoming momentum, global y-dir, only for iadvec = 1.
   real(kind=dp), allocatable, target, dimension(:) :: ucmag !< [m/s] Cell center velocity magnitude. {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, dimension(:) :: uc1D !< [m/s] 1D cell center velocities.
   real(kind=dp), allocatable, dimension(:) :: alpha_mom_1D !< [-] Ratio of incoming momentum versus initial estimate of outgoing momentum.
   real(kind=dp), allocatable, dimension(:) :: alpha_ene_1D !< [-] Ratio of incoming energy versus initial estimate of outgoing energy.
   real(kind=dp), allocatable, dimension(:) :: cfli !< [-] Sum of incoming courants (    ) = sum( Dt*Qj/Vi).
   real(kind=dp), allocatable, dimension(:) :: dvxc !< [m3/s2] Cell center stress term, global x-dir.
   real(kind=dp), allocatable, dimension(:) :: dvyc !< [m3/s2] Cell center stress term, global y-dir.
   real(kind=dp), allocatable, dimension(:) :: squ !< [m3/s] Cell center outgoing flux.
   real(kind=dp), allocatable, dimension(:) :: sqi !< [m3/s] Cell center incoming flux.
   real(kind=dp), allocatable, dimension(:) :: squ2D !< [m3/s] Cell center outgoing 2D flux.
   real(kind=dp), allocatable, dimension(:) :: sqwave !< [m3/s] Cell center outgoing flux, including gravity wave velocity (for explicit time-step).
   real(kind=dp), allocatable, dimension(:) :: squcor !< [m3/s] Cell center outgoing flux with some corrections to exclude structure links (if enabled).
   real(kind=dp), allocatable, dimension(:) :: hus !< [m2/s] hu averaged at 3D cell.
   real(kind=dp), allocatable, dimension(:) :: workx !< Work array.
   real(kind=dp), allocatable, dimension(:) :: worky !< Work array.
   real(kind=dp), allocatable, dimension(:, :) :: work0 !< Work array.
   real(kind=dp), allocatable, dimension(:, :) :: work1 !< Work array.
   real(kind=dp), allocatable, target, dimension(:) :: ucx_mor !< [m/s] Cell center velocity for sedmor, global x-dir. {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: ucy_mor !< [m/s] Cell center velocity for sedmor, global y-dir. {"location": "face", "shape": ["ndkx"]}

   real(kind=dp), allocatable, dimension(:) :: dsadx !< [ppt/m] Cell center sa gradient.
   real(kind=dp), allocatable, dimension(:) :: dsady !< [ppt/m] Cell center sa gradient.

   ! node related, dim = ndxi
   real(kind=dp), allocatable, target, dimension(:) :: freeboard !< [m] For output purposes: freeboard at cell center, only for 1D.
   real(kind=dp), allocatable, target, dimension(:) :: hsOnGround !< [m] For output purposes: waterdepth above ground level, only for 1D.
   real(kind=dp), allocatable, target, dimension(:) :: volOnGround !< [m3] For output purposes: volume above ground level, only for 1D.
   real(kind=dp), allocatable, dimension(:) :: qCur1d2d !< [m3/s] Total 1d2d net inflow, current discharge.
   real(kind=dp), allocatable, dimension(:) :: vTot1d2d !< [m3] Total 1d2d net inflow, cumulative volume.
   real(kind=dp), allocatable, dimension(:) :: qCurLat !< [m3/s] Total lateral net inflow, current discharge.
   real(kind=dp), allocatable, dimension(:) :: vTotLat !< [m3] Total lateral net inflow, cumulative volume.

   ! link related, dim = lnx
   real(kind=dp), allocatable, dimension(:) :: s1Gradient !< [-] For output purposes: water level gradient on flow links.

   ! Secondary Flow
   real(kind=dp), allocatable, dimension(:) :: ducxdx !< [1/s] Cell center gradient of x-velocity in x-dir.
   real(kind=dp), allocatable, dimension(:) :: ducxdy !< [1/s] Cell center gradient of x-velocity in y-dir.
   real(kind=dp), allocatable, dimension(:) :: ducydx !< [1/s] Cell center gradient of y-velocity in x-dir.
   real(kind=dp), allocatable, dimension(:) :: ducydy !< [1/s] Cell center gradient of y-velocity in y-dir.
   real(kind=dp), dimension(:), allocatable :: spircrv !< [1/m] 1/R_s streamline curvature.
   real(kind=dp), dimension(:), allocatable :: spirint !< [m/s] Spiral flow intensity.
   real(kind=dp), dimension(:), allocatable :: spirsrc !< [m/s2] Source term for spiral flow intensity.
   real(kind=dp), dimension(:), allocatable :: spirfx !< [m/s2] Secondary flow force for momentum in x-dir.
   real(kind=dp), dimension(:), allocatable :: spirfy !< [m/s2] Secondary flow force for momentum in y-dir.
   real(kind=dp), dimension(:), allocatable :: spirucm !< [m/s] Velocity in the flow node.
   real(kind=dp), dimension(:), allocatable :: ht_xx !< [-] Array hT_xx, for calculation of spirfx and spirfy.
   real(kind=dp), dimension(:), allocatable :: ht_xy !< [-] Array hT_xy, for calculation of spirfx and spirfy.
   real(kind=dp), dimension(:), allocatable :: czusf !< [-] Chezy coefficient on flow link.
   real(kind=dp), dimension(:), allocatable :: czssf !< [-] Chezy coefficient in flow node.
   real(kind=dp), dimension(:), allocatable :: fcoris !< [m/s2] Coriolis force in the flow node.

   real(kind=dp), dimension(:), allocatable :: spiratx !< [-] x component of normalised vector in direction of depth averaged velocity.
   real(kind=dp), dimension(:), allocatable :: spiraty !< [-] y component of normalised vector in direction of depth averaged velocity.

   real(kind=dp) :: spirE = 0.0_dp !< [-] Factor for weighing the effect of the spiral flow intensity on transport angle, Eq 11.45 of Delft3D manual.
   real(kind=dp) :: spirbeta = 0.0_dp !< [-] Factor for weighing the effect of the spiral flow on flow dispersion stresses, Eq 9.155 of Delft3D manual.
   integer :: numoptsf

   ! Anti-creep
   real(kind=dp), dimension(:), allocatable :: dsalL !< [ppt*m3/s] The flux of salinity on flow linkes for anti-creep.
   real(kind=dp), dimension(:), allocatable :: dtemL !< [degC*m3/s] The flux of temperature on flow nodes  for anti-creep.

   real(kind=dp), allocatable, target, dimension(:) :: sa0 !< [ppt] Salinity at start of timestep. {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: sa1 !< [ppt] Salinity at end of timestep. {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: satop !< [ppt] Salinity help in initialise, deallocated. {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target, dimension(:) :: sabot !< [ppt] Salinity help in initialise, deallocated. {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, dimension(:) :: supq !< [ppt*m3/s] Summed upwind salinity fluxes.
   real(kind=dp), allocatable, dimension(:) :: qsho !< [ppt*m3/s] Higher order part of upwind salinity fluxes.
   real(kind=dp), allocatable, target, dimension(:) :: tem0 !< [degC] water temperature at start of timestep. {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: tem1 !< [degC] water temperature at end of timestep. {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, dimension(:) :: qtho !< [ppt*m3/s] Higher order part of upwind temperature fluxes. {size=lnkx}

   real(kind=dp), allocatable, dimension(:) :: ww1 !< [m/s] Vertical velocity end of timestep.
   real(kind=dp), allocatable, dimension(:) :: qw !< [m3/s] Vertical flux through interface.
   real(kind=dp), allocatable, dimension(:, :) :: tidep !< [m2/s2] Tidal potential.
   real(kind=dp), allocatable, dimension(:) :: tidef !< [m/s2] Tidal force.
   real(kind=dp), allocatable, dimension(:) :: s1init !< [m] Initial water level, for correction in SAL.

   real(kind=dp), allocatable, dimension(:) :: vih !< [m2/s] Horizontal eddy viscosity in cell center.
   real(kind=dp), allocatable, dimension(:) :: qin !< [m3/s] Rain, evap, qlat and src netto inloop.

   ! Link related, dim = lnkx
   real(kind=dp), allocatable, target, dimension(:) :: u0 !< [m/s] Flow velocity at start of timestep.
   real(kind=dp), allocatable, target, dimension(:) :: u1 !< [m/s] Flow velocity at end of timestep. {"location": "edge", "shape": ["lnkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: u_to_umain !< [-] Factor for translating general velocity to the flow velocity in the main channel at end of timestep. {"location": "edge", "shape": ["lnkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: q1 !< [m3/s] Discharge at end of timestep n, used as q0 in timestep n+1, statement q0 = q1 is out of code, saves 1 array. {"location": "edge", "shape": ["lnkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: q1_main !< [m3/s] Discharge in main channel at end of timestep. {"location": "edge", "shape": ["lnkx"]}
   real(kind=dp), allocatable, dimension(:) :: qa !< [m3/s] Discharge used in advection, qa=au(n)*u1(n+1) instead of u1(n+1) alone.
   real(kind=dp), allocatable, dimension(:) :: map_fixed_weir_energy_loss !< [m2/s2] Fixed weir energy loss at end of timestep. {"location": "edge", "shape": ["lnkx"]}
   real(kind=dp), allocatable, dimension(:) :: cflj !< [-] Courant number for link j to downwind volume i = Dt*Qj/Vi.
   real(kind=dp), allocatable, dimension(:) :: tetaj !< [-] 1-1/sum(upwind incoming courants).
   real(kind=dp), allocatable, target, dimension(:) :: au !< [m2] Flow area at u point. {"location": "edge", "shape": ["lnkx"]}
   real(kind=dp), allocatable, target, dimension(:) :: au_nostrucs !< [m2] Flow area at u point without structures. {"location": "edge", "shape": ["lnkx"]}
   real(kind=dp), allocatable, dimension(:) :: ucxu !< [m/s] Upwind link ucx.
   real(kind=dp), allocatable, dimension(:) :: ucyu !< [m/s] Upwind link ucy.
   real(kind=dp), allocatable, dimension(:, :) :: au1D !< [m2] Cross-sectional area at begin and end of 1D link (only relevant for Pure1D).
   real(kind=dp), allocatable, dimension(:, :) :: wu1D !< [m] Surface width at begin and end of 1D link (only relevant for Pure1D).
   real(kind=dp), allocatable, dimension(:, :) :: sar1D !< [m2] Surface area of first and second half of 1D link (only relevant for Pure1D).
   real(kind=dp), allocatable, dimension(:) :: volu1D !< [m3] Volume of 1D link (only relevant for Pure1D).
   real(kind=dp), allocatable, dimension(:) :: u1Du !< [m/s] Upwind 1D link velocity (only relevant for Pure1D).
   real(kind=dp), allocatable, dimension(:, :) :: q1D !< [m3/s] Discharge at begin and end of 1D link (only relevant for Pure1D).
   integer, allocatable, dimension(:, :) :: isnbnod !< [-] Sign of left/right node follows your dir in jaPure1D assumptions, -1 or 1 for Ja1D nodes.
   integer, allocatable, dimension(:, :) :: isnblin !< [-] Sign of left/right link follows your dir in jaPure1D assumptions, -1 or 1 for Ja1D nodes.
   real(kind=dp), allocatable, dimension(:) :: advi !< [1/s] Advection implicit part.
   real(kind=dp), allocatable, dimension(:) :: adve !< [m/s2] Advection explicit part.
   real(kind=dp), allocatable, dimension(:) :: adve0 !< [m/s2] Advection explicit part prevstep.
   real(kind=dp), allocatable, target, dimension(:) :: hu !< [m] Upwind waterheight at u-point; for 3D layers the distance from the top of layer to the bed. {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), allocatable, dimension(:) :: huvli !< Inverse alpha weighted waterheight at u-point (volume representative).
   real(kind=dp), allocatable, dimension(:) :: v !< [m/s] Tangential velocity in u point.
   real(kind=dp), allocatable, dimension(:) :: suu !< [m/s2] Stress u dir.
   real(kind=dp), allocatable, dimension(:) :: cfuhi !< [1/m] g/(hCC) u point.
   real(kind=dp), allocatable, target, dimension(:) :: frcu !< [-] Friction coefficient set by initial fields. {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), allocatable, dimension(:) :: frcu_mor !< [-] Friction coefficient in morphologically active region set by initial fields. {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), allocatable, dimension(:) :: frcu_bkp !< [-] Backup of friction coefficient set by initial fields. {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), allocatable, dimension(:) :: cfclval !< [-] Array for calibration factor for friction coefficients.
   real(kind=dp), allocatable, dimension(:, :) :: cftrt !< [-] Array for friction coefficients due to trachytopes.
   real(kind=dp), allocatable, target, dimension(:) :: cftrtfac !< [-] Array for optional multiplication factor for trachytopes's returned roughness values.
   integer :: jacftrtfac !< [-] Whether or not (1/0) a multiplication factor field was specified for trachytopes's Chezy roughness values.
   real(kind=dp), allocatable, dimension(:) :: czu !< [-] Array for chezy friction at flow links. {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), allocatable :: frcu0(:) !< [-] Array for Manning friction at flow links at start of the run. {"location": "edge", "shape": ["lnx"]}
   logical, allocatable :: dynveg(:) !< [-] Vegetation present or not. {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), allocatable, target, dimension(:) :: frculin !< [-] Friction coefficient set by initial fields.
   integer, allocatable, dimension(:) :: ifrcutp !< [-] Friction coefficient type initial fields.
   real(kind=dp), allocatable, target, dimension(:) :: Cdwusp !< [-] Wind friction coefficient at u point set by initial fields.
   real(kind=dp), allocatable, dimension(:) :: wind_speed_factor !< [-] Wind speed multiplication factor.
   real(kind=dp), allocatable, dimension(:) :: solar_radiation_factor !< [-] Solar radiation multiplication factor.
   real(kind=dp), allocatable, dimension(:) :: z0ucur !< [-] Current related roughness, moved from waves, always needed.
   real(kind=dp), allocatable, dimension(:) :: z0urou !< [-] Current and wave related roughness.

   real(kind=dp), allocatable, dimension(:) :: frcuroofs !< [-] Temp.

   real(kind=dp), allocatable, target, dimension(:) :: frcInternalTides2D !< [-] Internal tides friction coefficient gamma, tau/rho = - gamma u.grad h grad h.

   real(kind=dp), allocatable, dimension(:) :: wavfu !< [-] Wave force u point.
   real(kind=dp), allocatable, dimension(:) :: wavfv !< [-] Wave force v point.
   real(kind=dp), allocatable, dimension(:) :: wdsu !< [m2/s2] Windstress/rhow u point.
   real(kind=dp), allocatable, target, dimension(:) :: wdsu_x !< [N/m2] Windstress u point x-component
   real(kind=dp), allocatable, target, dimension(:) :: wdsu_y !< [N/m2] Windstress u point y-component
   real(kind=dp), allocatable, target, dimension(:) :: w_star !< [m/s] Free convective velocity scale at cell centers.
   real(kind=dp), allocatable, target, dimension(:) :: obukhov_length !< [m] Obukhov length at cell centers.
   real(kind=dp), allocatable, target, dimension(:) :: transfer_coeff_momentum !< [-] Bulk transfer coefficient for momentum flux at cell centers.
   real(kind=dp), allocatable, target, dimension(:) :: transfer_coeff_sensible_heat !< [-] Bulk transfer coefficient for sensible heat flux at cell centers.
   real(kind=dp), allocatable, target, dimension(:) :: transfer_coeff_latent_heat !< [-] Bulk transfer coefficient for latent heat flux at cell centers.
   real(kind=dp), allocatable, dimension(:) :: wavmubnd !< [m2/s] Wave-induced mass flux (on open boundaries).
   integer :: number_steps_limited_visc_flux_links = 0 !< [-] Number of steps with limited viscosity/flux on links.
   integer, parameter :: MAX_PRINTS_LIMITED_VISC_FLUX_LINKS = 10 !< [-] Number of messages in dia file on limited viscosity/flux links.
   real(kind=dp), allocatable, dimension(:) :: vicLu !< [m2/s] Horizontal eddy viscosity coefficient at u point (limited only if ja_timestep_auto_visc==0).
   real(kind=dp), allocatable, dimension(:) :: viu !< [m2/s] Horizontal eddy viscosity coefficient at u point, modeled part of viscosity = vicLu - viusp.
   real(kind=dp), allocatable, target, dimension(:) :: viusp !< [m2/s] User defined spatial eddy viscosity coefficient at u point. {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), allocatable, target, dimension(:) :: diusp !< [m2/s] User defined spatial eddy diffusivity coefficient at u point. {"location": "edge", "shape": ["lnx"]}
                                                             !< So in transport, total diffusivity = viu*sigdifi + diusp
   real(kind=dp), allocatable, dimension(:) :: fcori !< [1/s] Spatially variable fcorio coeff at u point.
   real(kind=dp), allocatable, dimension(:) :: fvcoro !< [m/s2] 3D adamsbashford u point.

   real(kind=dp), allocatable, dimension(:) :: plotlin !< [-] For plotting on u points.
   integer, allocatable, dimension(:) :: numlimdt !< [-] Number of times this point was the timestep limiting point.
   integer :: numlimdt_baorg = 0 !< [-] Number of times limiting > numlimdt_baorg, keep original ba.
   real(kind=dp) :: baorgfracmin = 0 !< [-] ba = max(cutarea, ba*baorgfracmin).

   real(kind=dp), allocatable, dimension(:) :: zn2rn !< [-] Weight from zn to rn, flownode to netnode.

   real(kind=dp), allocatable, target, dimension(:) :: tausx !< [-] Vector components shear stress.
   real(kind=dp), allocatable, target, dimension(:) :: tausy !< [-] Vector components shear stress.
   real(kind=dp), allocatable, target, dimension(:) :: taubxu !< [-] Maximal bed shear stress.
   real(kind=dp), allocatable, target, dimension(:) :: taubu !< [-] Mean bed shear stress.
   real(kind=dp), allocatable, dimension(:) :: q1waq !< [m3/s] Cumulative q1 within current waq-timestep.
   real(kind=dp), allocatable, dimension(:) :: qwwaq !< [m3/s] Cumulative qw within current waq-timestep.

   ! solving related, dim = ndx for 2D, otherwise ndx*kmxd
   real(kind=dp), allocatable, dimension(:) :: fu !< [-] Main diagonal. {size=lnx}
   real(kind=dp), allocatable, dimension(:) :: ru !< [-] rhs. {size=lnx}
   real(kind=dp), allocatable, dimension(:) :: bb !< [-] Main diagonal. {size=ndx}
   real(kind=dp), allocatable, dimension(:) :: dd !< [-] rhs. {size=ndx}

   integer, allocatable, dimension(:) :: struclink

   ! basis
   real(kind=dp) :: vol0tot !< [m3] Total volume start of timestep.
   real(kind=dp) :: vol1tot !< [m3] Total volume   end of timestep.
   real(kind=dp) :: vol1ini !< [m3] Total volume   initially.
   real(kind=dp) :: vol1icept !< [m3] Total volume interception end of timestep.
   real(kind=dp) :: Volgrw !< [m3] Total volume grw end of timestep.
   real(kind=dp) :: Volgrwini !< [m3] Total volume grw initially.

   real(kind=dp) :: qinbnd !< [m3/s] Actual influx boundaries.
   real(kind=dp) :: qoutbnd !< [m3/s] Actual outflux boundaries.
   real(kind=dp) :: qincel !< [m3/s] Actual influx cells.
   real(kind=dp) :: qoutcel !< [m3/s] Actual outflux cells.

   real(kind=dp) :: vinbnd !< [m3] Volume in  boundaries of timestep.
   real(kind=dp) :: voutbnd !< [m3] Volume out boundaries of timestep.
   real(kind=dp) :: vincel !< [m3] Volume in  cells      of timestep.
   real(kind=dp) :: voutcel !< [m3] Volume out cells      of timestep.
   real(kind=dp) :: volerr !< [m3] Volume error of timestep vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel.

   real(kind=dp) :: vinbndcum !< [m3] Cumulative volume through boundaries in. Cumulative values
   real(kind=dp) :: voutbndcum !< [m3] Cumulative volume through boundaries out.
   real(kind=dp) :: vincelcum !< [m3] Cumulative volume in  cells. Actual values
   real(kind=dp) :: voutcelcum !< [m3] Cumulative volume out cells.
   real(kind=dp) :: volerrcum !< [m3] Volume error since start of computation.

   real(kind=dp) :: dvolbot !< [m3] Associated with jamorf.

   ! extra
   real(kind=dp) :: qinrain !< [m3/s] Total influx rain.
   real(kind=dp) :: qinrainground !< [m3/s] Total influx rain onto the ground.
   real(kind=dp) :: qouteva !< [m3/s] Total outflux evaporation.
   real(kind=dp) :: qoutevaicept !< [m3/s] Total outflux evaporation from interception layer.
   real(kind=dp), dimension(2) :: qinlat !< [m3/s] Total influx diffuse laterals (1D and 2D).
   real(kind=dp), dimension(2) :: qoutlat !< [m3/s] Total outflux diffuse laterals (1D and 2D).
   real(kind=dp) :: qingrw !< [m3/s] Total influx groundwater.
   real(kind=dp) :: qoutgrw !< [m3/s] Total outflux groundwater.
   real(kind=dp) :: qinsrc !< [m3/s] Total influx local point sources.
   real(kind=dp) :: qoutsrc !< [m3/s] Total outflux local pount sources.
   real(kind=dp), dimension(2) :: qinext !< [m3/s] Total influx Qext (1D and 2D).
   real(kind=dp), dimension(2) :: qoutext !< [m3/s] Total outflux Qext(1D and 2D).

   real(kind=dp) :: vinrain !< [m3] Total volume in  rain in the last time step.
   real(kind=dp) :: vinrainground !< [m3] Total volume of rain falling onto the ground (in the last time step).
   real(kind=dp) :: vouteva !< [m3] Total volume out evaporation.
   real(kind=dp) :: voutevaicept !< [m3] Total volume out evaporation from interception layer.
   real(kind=dp), dimension(2) :: vinlat !< [m3] Total volume in  diffuse laterals (1D and 2D).
   real(kind=dp), dimension(2) :: voutlat !< [m3] Total volume out diffuse laterals (1D and 2D).
   real(kind=dp) :: vingrw !< [m3] Total volume in  groundwater.
   real(kind=dp) :: voutgrw !< [m3] Total volume out groundwater.
   real(kind=dp) :: vinsrc !< [m3] Total volume in  local point sources.
   real(kind=dp) :: voutsrc !< [m3] Total volume out local pount sources.
   real(kind=dp), dimension(2) :: vinext !< [m3] Total volume in  Qext (1D and 2D).
   real(kind=dp), dimension(2) :: voutext !< [m3] Total volume out Qext (1D and 2D).

   real(kind=dp) :: vinraincum !< [m3] Total inflow from rain integrated over all time steps.
   real(kind=dp) :: voutevacum !< [m3] Total outflow to evaporation.
   real(kind=dp), dimension(2) :: vinlatcum !< [m3] Total inflow from diffuse laterals (1D and 2D).
   real(kind=dp), dimension(2) :: voutlatcum !< [m3] Total outflow to diffuse laterals  (1D and 2D).
   real(kind=dp) :: vingrwcum !< [m3] Total inflow from groundwater.
   real(kind=dp) :: voutgrwcum !< [m3] Total outflow to groundwater.
   real(kind=dp) :: vinsrccum !< [m3] Total inflow from local point sources.
   real(kind=dp) :: voutsrccum !< [m3] Total outflow to local pount sources.
   real(kind=dp), dimension(2) :: vinextcum !< [m3] Total inflow from Qext (1D and 2D).
   real(kind=dp), dimension(2) :: voutextcum !< [m3] Total outflow to  Qext (1D and 2D).

   real(kind=dp) :: DissInternalTides !< [J/s] Total internal tides dissipation.
   real(kind=dp), allocatable, dimension(:) :: DissInternalTidesPerArea !< [J/m2*s] Internal tides dissipation / area.
   real(kind=dp) :: GravInput !< [J/s] Total gravitational input (incl. SAL).
   real(kind=dp) :: SALInput !< [J/s] Total SAL input.
   real(kind=dp) :: SALInput2 !< [J/s] Total SAL input, different formulation.

   real(kind=dp) :: a0tot !< [m2] Total wet surface area start of timestep.
   real(kind=dp) :: a1tot !< [m2] Total wet surface area   end of timestep.
   real(kind=dp) :: a1ini !< [m2] Total model area rain evap.
   real(kind=dp) :: hsaver !< [m] Average waterdepth, vol/are.

   ! basis zout
   real(kind=dp) :: sam0tot !< [ppt*m3] Total mass start of timestep.
   real(kind=dp) :: sam1tot !< [ppt*m3] Total mass end of timestep.

   real(kind=dp) :: saminbnd !< [ppt*m3] Actual mass in  boundaries of timestep.
   real(kind=dp) :: samoutbnd !< [ppt*m3] Actual mass out boundaries of timestep.
   real(kind=dp) :: samerr !< [m3] vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel.

   real(kind=dp) :: difmaxlev !< [m] Max lev diff.
   real(kind=dp) :: epsmaxlev = 1.0e-8_dp !< [m] Eps lev diff.
   real(kind=dp) :: epsmaxlevm = 1.0e-8_dp !< [m] Eps lev diff minus part.

   logical :: validateon !< [-] Should we validate flow state yes or no (switched off at water drop).
   integer :: noddifmaxlev !< [-] Node number of max lev diff.
   integer :: nodneg !< [-] Node number with negative hs.
   integer :: numnodneg !< [-] Number of posh checks.
   integer :: Linkdriedmx = 0 !< [-] Maximum number of au growth steps after having dried.
   integer :: nodnegtek !< [-] Node number with negative hs to draw.
   integer :: kkcflmx !< [-] 2D Node number with max courant.
   integer :: kcflmx !< [-] 3D Node number with max courant.
   integer :: itsol !< [-] Act number of iterations in solve.
   integer :: ndmin !< [-] Node number where min znod is found in viewing area.
   integer :: ndmax !< [-] Node number where max znod is found in viewing area.
   integer :: Lnmin !< [-] Link number where min zlin is found in viewing area.
   integer :: Lnmax !< [-] Link number where max zlin is found in viewing area.

   integer, parameter :: MAX_IDX = 40
   real(kind=dp), dimension(MAX_IDX) :: volcur !< [m3] Volume totals in *current* timestep only (only needed for MPI reduction).
   real(kind=dp), dimension(MAX_IDX) :: cumvolcur = 0.0_dp !< [m3] Cumulative volume totals starting from the previous His output time, cumulate with volcur (only needed for MPI reduction).
   real(kind=dp), dimension(MAX_IDX), target :: voltot
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

   logical :: ucxyq_read_rst !< Determines if variables `ucxq` and `ucxy` have been read from restart.

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
      layertype = LAYTP_SIGMA !< 1 = sigma-layers, 2 = z- or z-sigma-layers, 3 = polygon defined mixed layers, 4 = density controlled sigma-layers
      iturbulencemodel = TURBULENCE_MODEL_KEPS !< 0 = none, 1 = constant, 2 = algebraic, 3 = k-eps, 4 = k-tau
      ieps = 2 !< bottom boundary type eps. eqation, 1=dpmorg, 2 = dpmsandpit, 3=D3D, 4=Dirichlethdzb
      z_layer_growth_factor = 1.0_dp

      ! Remaining of variables is handled in reset_flow()
      call reset_flow()
   end subroutine default_flow

!> Resets only flow variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_flow() instead.
   subroutine reset_flow()
      use m_missing, only: dmiss
! node related

! basis
      vol0tot = 0 ! total volume start of timestep          (m3)
      vol1tot = 0 ! total volume   end of timestep          (m3)
      vol1icept = 0.0_dp ! total volume interception end of timestep (m3)
      vol1ini = -1.0_dp ! total volume   initially                (m3)
      Volgrw = 0 ! total grw volume                        (m3)
      Volgrwini = 0.0_dp ! total grw volume initially              (m3)

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

      dvolbot = 0.0_dp !     (m3)

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

      DissInternalTides = 0.0_dp !< total Internal Tides Dissipation (J/s)

      a0tot = 0 ! total wet surface area start of timestep (m2)
      a1tot = 0 ! total wet surface area   end of timestep (m2)
      a1ini = 0 ! total model       area                   (m2)
      hsaver = 0 ! average waterdepth (m), vol/are

      difmaxlev = 0 ! max lev diff (m3)

      validateon = .true. ! validate flow state yes or no

      nodneg = 0 ! node nr with negative hs
      kkcflmx = 0 ! 2D node nr with max courant
      itsol = 0 ! act nr. of iterations in solve
      numnodneg = 0 ! nr of posh checks
      ndmin = 0 ! node nr where min znod is found in viewing area
      ndmax = 0 ! node nr where max znod is found in viewing area
      Lnmin = 0 ! link nr where min zlin is found in viewing area
      Lnmax = 0 ! link nr where max zlin is found in viewing area

      sam0tot = 0.0_dp !< Total mass start of timestep            (m3ppt)
      sam1tot = 0.0_dp !< Total mass   end of timestep            (m3ppt)
      samerr = 0.0_dp !< vol1tot - vol0tot - vinbnd + voutbnd - vincel + voutcel   (m3)

      voltot(:) = 0.0_dp
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

   !> Check if salinity, temperature or sediment are simulated, i.e. density needs to be incorporated
   pure function use_density() result(res)
      use m_flowparameters, only: jasal, temperature_model, TEMPERATURE_MODEL_NONE, jased

      logical :: res !< Return value

      res = (jasal > 0 .or. temperature_model /= TEMPERATURE_MODEL_NONE .or. jased > 0)
   end function use_density

   subroutine map_ndkx_to_ndx()
      use m_cell_geometry, only: ndx
      use m_alloc, only: realloc
      
      integer :: n, k

      call realloc(ndkx_to_ndx, ndkx)

      ! Fill the surface layer
      do n = 1, ndx
         ndkx_to_ndx(n) = n
      end do

      ! Fill the 3D layers
      if (kmx > 0) then
         do n = 1, ndx
            do k = kbot(n), kbot(n) + kmxn(n) - 1
               ndkx_to_ndx(k) = n
            end do
         end do
      end if
   end subroutine map_ndkx_to_ndx

end module m_flow
