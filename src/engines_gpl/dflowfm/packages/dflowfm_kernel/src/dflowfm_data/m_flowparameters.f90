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

module m_flowparameters
   use precision, only: dp
   use m_sediment, only: jased
   use m_missing

   implicit none

   integer :: itstep !< time step 0=no, 1 =step_explicit, 2=step_reduce, 3=step_jacobi, 4: explicit
   integer :: iadvec !< adv type, 0=no, 1 = Wenneker vol, qu-udzt array, 2=1, function,
   !< 3 =Perot in uit, 4 =Perot in, explicit
   !< 5 =Perot in uit, 6 =Perot in, piaczek teta
   !< 7 =Perot in uit, 8 =Perot in, piaczek fully implicit
   !< 9 =Perot in uit, 10=Perot in, piaczek fully implicit, weir + factor
   !< 11=Perot in uit, 12=Perot in, piaczek fully implicit, gate + factor
   !< 20=Energy conserving compact, piaczek fully implicit, weir
   integer :: maxNonlinearIterations !< maximal iterations in non linear iteration loop before a time step reduction is applied
   logical :: setHorizontalBobsFor1d2d !< bobs are set to 2d bedlevel, to prevent incorrect storage in sewer system.
   logical :: dxDoubleAt1DEndNodes !< indicaties whether a 1D grid cell at the end of a network has to be extended with 0.5*dx
   integer :: iadvec1D !< same, now for 1D links
   integer :: iadveccorr1D2D !< Advection correction of 1D2D link volume (0: none, 1: link volume au*dx')

   logical :: changeVelocityAtStructures !< Set the flow velocity at structures in setucxucyucxuucyu to the
   !< flow velocity upstream of the structure
   logical :: changeStructureDimensions !< Change the crestwidth of a structure, in case the crestwidth is larger than the
   !< wet surface width and make sure the crest level is equal or larger than the
   !< bed level of the channel.
   integer :: lincontin !< 0 = no, 1 = yes linear continuity

   integer :: Perot_type !< Perot weigthing type of cell center velocities ucx, ucy
                                                        !! in vectors:
                                                        !! 0 : uc*sum(w) = sum (u W)
                                                        !! 1 : uc*A      = sum(u dxa W)
                                                        !! 2 : uc*A*hs   = sum(u dxa W hu ), ie waterdepth dependent
   ! To do: Check unused options (UNST-8641)
   ! 2 : uc*V      = sum(q dxa      ), ie waterdepth dependent
   ! 3 : uc*A*humx = sum(u dxa W hu ), humx = max(hu)
   ! 4 : uc*A*humx = sum(u dxa W hu ), humx = max(hu)
   ! 5 : uc*Vc     = sum(u dxa W hu ), Vc = dxa W hu based volume in cell
   ! 6 : as 5, also for Coriolis
   integer, parameter :: NOT_DEFINED = -1
   integer, parameter :: PEROT_WIDTH_BASED = 0 !< uc*sum(w) = sum (u W)
   integer, parameter :: PEROT_AREA_BASED = 1 !< uc*A      = sum(u dxa W)
   integer, parameter :: PEROT_VOLUME_BASED = 2 ! uc*A*hs   = sum(u dxa W hu ), ie waterdepth dependent

   integer :: Perot_weight_update !> Perot weight update for 1D nodes (0: no (default), 1: yes)

   integer :: jacomp = 1 !! same now for netnodes, 0 = default, 1 = use cs, sn in weighting, 2=regular scalar x,y interpolation based on banf

   integer :: autotrisam = 1 !< Generate triangles after generating samplles in polygon (0:no 1:yes)

   integer :: icorio !< Coriolis weigthing

   integer :: newcorio = 1 !< 0=prior to 27-11-2019, 1=no normal forcing on open bnds, plus 12 variants

   integer :: jacorioconstant = 0 !< 0=default, 1=Coriolis constant in sferic models anyway,2=beta plane, both in cart. and spher. coor.

   real(kind=dp) :: Oceaneddyamp = 0.0d0 !< Amplitude of testcase Oceaneddy, negative = anticyclone

   real(kind=dp) :: Oceaneddyvel = 0.0d0 !< Velocity of testcase Oceaneddy, negative = anticyclone

   real(kind=dp) :: Oceaneddysizefrac = 0.05d0 !< Length scale relative to diagonal domain size

   real(kind=dp) :: Oceaneddysize = 0.0d0 !< Length scale relative to diagonal domain size

   real(kind=dp) :: Oceaneddyxoff = 0.0d0 !< relative domain size xoffset from centre

   real(kind=dp) :: Oceaneddyyoff = 0.0d0 !< relative domain size yoffset from centre

   real(kind=dp) :: Corioadamsbashfordfac = 0.5d0 !< Coriolis Adams Bashforth , 0d0 = explicit, 0.5 = AB

   real(kind=dp) :: hhtrshcor !< if > 0 safety for hu/hs in corio for now, ==0

   real(kind=dp) :: trshcorio !< below this depth coriolis force scaled down linearly to 0

   integer :: jatidep !< use tide potential forcing yes no

   integer :: jaselfal !< use self attraction and loading yes no
   integer :: jaSELFALcorrectWLwithIni !< correct water level with initial water level in SAL

   real(kind=dp) :: doodsonstart, doodsonstop, doodsoneps

   integer :: jatrt !< Trtrou = #Y# --> 1 , Trtrou = #N# --> 0  (Delft3D style input)

   integer :: jacali !< use calibration factors (0 = no, 1 = yes)

   integer :: jasal !< Include salinity set in mdf

   integer :: jatem !< Temperature model (0=no, 5=heatfluxmodel)

   integer :: janudge !< temperature and salinity nudging
   integer :: jainiwithnudge !< initialize salinity and temperature with nudge variables

   integer :: itempforcingtyp !< Forcing parameter types 1,2 humidity, 3,4 dewpoint see code

   logical :: btempforcingtypA !< Forcing parameter Air temperature is given as a separate field or not
   logical :: btempforcingtypC !< Forcing parameter Cloudiness given as a separate field or not
   logical :: btempforcingtypH !< Forcing parameter Humidity given as a separate field or not
   logical :: btempforcingtypS !< Forcing parameter Solarradiation given as a separate field or not
   logical :: btempforcingtypL !< Forcing parameter Long wave radiation given as a separate field or not

   integer :: jarhoxu !< rho effects in momentum, 0=no, 1=in horizontal adv, 2=+ in vertical adv, 3 = + in pressure term

   integer :: jawave !< Include wave model nr, 0=no, 1=fetchlimited hurdle stive + swart, 3=SWAN, 4=XBeach wave driver, 5=Const, 6=SWAN-NetCDF, 7=Offline Wave Coupling

   integer :: waveforcing !< Wave forcing type, 0=no, 1=based on radiation stress gradients, 2=based on dissipation, NOT implemented yet, 3=based on dissipation at free surface and water column, NOT implemented yet

   logical :: flowWithoutWaves = .false. !< True: Do not use Wave data in the flow computations, it will only be passed through to D-WAQ

   integer :: jawavestreaming !< Switch on in D3D model: >=1 : streaming mom , >= 2 : streaming mom + turb

   integer :: jawaveStokes !< Vertical Stokes profile: 0=no, 1 = uniform, 2 = second order Stokes profile

   integer :: jawavebreakerturbulence !< Add wave-induced production terms in turbulence modelling: 0 = no, 1 = yes

   integer :: jawavedelta = 1 !< Wave boundary layer formulation: 1=Sana 2007

   integer :: jawaveforces !< Apply wave forces to model (1, default), or not (0)

   integer :: jawaveSwartDelwaq !< communicate to Delwaq taucur + tauwave instead of taucur

   integer :: modind = 1 !< Nr of wave-current bed friction model, 9 = vanrijn, 1 = fredsoe, etc like d3d

   integer :: jaavgwavquant = 0 !< 0=no time avg'd output for jawave==4; 1=time avg'd output for jawave==4. Avg'ing period = ti_avgwav

   integer :: ihorvic !< 0=no visc, 1=do visc

   integer :: jacreep !< Include anti-creep calculation, (0=no, 1=yes)

   integer :: jasecflow !< 0: no, 1: yes

   integer :: japillar !< 0: no, 1: yes

   integer :: jaequili !< secondary flow intensity gets calculated as equilibrium (0=no, 1=yes)

   integer :: javiusp !< if 1 spatially varying horizontal eddy viscosity

   integer :: jadiusp !< if 1 spatially varying horizontal eddy viscosity

   integer :: jaCdwusp !< if 1 spatially varying windstress coefficient

   integer :: ja_wind_speed_factor !< if 1 wind speed multiplication factor is used

   integer :: ja_solar_radiation_factor !< if 1 solar radiation multiplication factor is used

   integer :: ja_friction_coefficient_time_dependent !< spatially and time dependent friction coefficient

   integer :: javiuplus3D = 1 !< add vertical eddy viscosity to horizontal eddy viscosity (1 = yes, 0 = no)

   integer :: jafrculin !< use linear friction yes/no

   integer :: jaFrcInternalTides2D !< use internal tides friction (1) or not (0)

   integer :: iuvfield !< intialise this velocityfield: 0 = no
                                                        !! 1:u=y**2, 2:idem, 60 deg, 3:rotation, 4=lin, 5=lin 60 deg

   integer :: istresstyp !< 1 : full stress tensor, semi  link oriented horvic2
                                                        !! 2 : full stress tensor, fully link oriented dvxc = ok and fast
                                                        !! 3 : 2, volume weighted
                                                        !! 4 : full node oriented
                                                        !! 5 : 4, volume weighted

   integer :: irov !< 0 : free slip
                                                        !! 1 : partial slip
                                                        !! 2 : no slip
                                                        !! 3 : glass  (mind you, in D3DFLOW 3 means free slip)

   integer :: ibedlevmode !< 1 : See BLMODE_DFM
                                                        !! 2 : See BLMODE_D3D
   integer, parameter :: BLMODE_DFM = 1 !< ibedlevmode value: Compute bed levels solely by ibedlevtyp, i.e., derived from velocity points (or direct bl tiles)
   integer, parameter :: BLMODE_D3D = 2 !< ibedlevmode value: Compute bed levels as D3D, i.e., derived from corner point depths. Currently always deepest (== DPSOPT=MAX).

   integer :: ibedlevtyp !< 1 : Bed levels at waterlevel cells (=flow nodes), like tiles xz, yz, bl , bob = max(bl left, bl right)
                                                        !! 2 : Bed levels at velocity points  (=flow links),            xu, yu, blu, bob = blu,    bl = lowest connected link
                                                        !! 3 : Bed levels at velocity points  (=flow links), using mean network levels xk, yk, zk  bl = lowest connected link
                                                        !! 4 : Bed levels at velocity points  (=flow links), using min  network levels xk, yk, zk  bl = lowest connected link

   integer :: ibedlevtyp1D !< 1 : same, 1D, 1 = tiles, xz(flow)=zk(net), bob(1,2) = max(zkr,zkl) , 3=mean netnode based

   integer :: izbndpos !< 0 : waterlevel boundary location as in D3DFLOW, 1=on network boundary, 2=on specified boundary polyline

   real(kind=dp) :: blmeanbelow !<  : if not -999d0, below this level the cell centre bedlevel is the mean of surrouding netnodes
   real(kind=dp) :: blminabove !<  : if not -999d0, above this level the cell centre bedlevel is the min of surrouding netnodes
   real(kind=dp) :: blmin !<  : lowest bedlevel point in model

   real(kind=dp) :: upot0 = -999d0 !<  : initial potential energy
   real(kind=dp) :: ukin0 = -999d0 !<  : initial potential energy

   integer :: jaupdbndbl !< Update bl at boundary (1 = update, 0 = no update)
   integer :: jaupdbobbl1d !< Update bl and bobs for 1d network (call to setbobs_1d only at initialization)

   integer :: nonlin !< 1 : non-linear continuity , == max(nonlin, nonlin2D) , 2 == pressurized nonlin
   integer :: nonlin1D !< 1 : non-linear continuity eq for 1D points, only for non-rectangles
   integer :: nonlin2D !< 1 : non-linear continuity eq for 2D points, only for skewed bed, i.e. jaconveyance2D >= 1

   integer :: iproftypuni !< 1 : circle, 2 : rectan, 3 = rectan R=H, negative = closed for rain and grw
   integer :: iproftypuni5 !< idem, for streetinlets
   integer :: iproftypuni7 !< idem for roofgutterpipes
   real(kind=dp) :: slotw2D !< minimum slotwidth 2D
   real(kind=dp) :: slotw1D !< minimum slotwidth 1D

   integer :: jaconveyance2D !< 1 : yes, 0 : no
   integer :: jaconveyance3D = 0 !< 1 : yes, 0 : no
   integer :: nums1it !<   : nr of non-linear continuity iterations
   integer :: nums1mit !<   : nr of non-linear continuity iterations outer loop ic nested
   integer :: isimplefixedweirs !< 1=only links stored, 0=complete crossection paths stored

   integer :: iNormalMethod !< 0: take normal in direction of flowlinks "1-2", 1: take normal perpendicular to netlinks "3-4"
   integer :: jaimplicit !< implicit momentum eqn. (1) or not (0)
   integer :: jafilter !< apply horizontal filter (1:explicit, 2:implicit) or not (0)
   integer :: filterorder !< order of filter
   integer :: jacheckmonitor !< compute and output "checkerboard" mode monitor

   real(kind=dp) :: Uniformhu !< Uniformhu for arjen's membranes
   real(kind=dp) :: bedslope !< bed inclination testcases
   real(kind=dp) :: bedslopedir = 45d0 !< bed inclination dir (deg)
   real(kind=dp) :: bedwidth = 100d0 !< width of channel
   real(kind=dp) :: bedwaveamplitude = 0d0 !< bed testcases
   real(kind=dp) :: bedwavelength = 0d0 !< bed testcases

   real(kind=dp) :: Slopedrop2D !< Apply losses for 'rain from the roof', only if local bottom slope > Slopedrop2D, only for Slopedrop2D  > 0.0
   logical :: drop1D !< Apply losses for all 1d links,
   real(kind=dp) :: drop2D !< Apply losses in 2D if downwind z below bob + 2/3 hu
   real(kind=dp) :: drop3D !< Apply losses in 3D if downwind z below bob + 2/3 hu
   real(kind=dp) :: zwsbtol = 0d0 !< zws(kb0) = bl - zwsbtol
   integer :: keepzlayeringatbed = 2 !< only for z, 0=thin bed layer
   !< 1= : bedlayer=zlayer
   !< 2= : 0.5*(z2+z0), z0 being floor level layer 1, z2 being ceiling layer 2
   !< 3= : 0.5*(z2+z0)
   !< 4= : max(z1,0.5*(z2+z0)), z1 being basic z-level of layer 1
   !< 5= : max(z1,0.9*z2+0.1*z0), just another smooth recipe
   integer :: ihuz = 4 !< 1= : central from bed til second or first above local bob (in sethu)
   !< 2= : all central
   !< 3= : central from bed till highest layer with equal levels
   integer :: ihuzcsig = 3 !< 1= : sig = 0.5*(Leftsig,Rightsig)                       (in sethu)
   !< 2= : sig = max (Leftsig,Rightsig)
   !< 3= : sig = min (Leftsig,Rightsig)
   !< 4= : sig = dble(LL-Lb+1) / dble(LLbc-Lb+1), uniform independent of L,R
   integer :: keepzlay1bedvol = 0 !< 1=: Correct bed volumes for keepzlayeringatbed=1
   !< 0=: default, consistent volumes transport and baroclinic terms
   real(kind=dp) :: cflmx !< max Courant nr ()
   real(kind=dp) :: cflw !< wave velocity fraction, total courant vel = u + cflw*wavevelocity
   real(kind=dp) :: teta0 !< 1.00d0   ! .52      ! uniform teta in horizontal (),
   integer :: ivariableteta !< 0=fully implicit,   1=teta constant,        2=variable teta
                                                        !! (set teta=1.0)      (set teta=0.51->0.99)   (set teta<0)
   integer :: japiaczek33 = 1 ! testing 1 2

   integer :: jacstbnd !< Delft-3D type cell-centered velocities at boundaries (ucx, ucy)
   !< or more precise: copy of inside cell vector, WITHOUT taking bnd. normal comp.
   integer :: jaLogprofatubndin !< ubnds inflow: 0=uniform U1, 1 = log U1, 2 = log U1 and k-eps accordingly
   integer :: jaLogprofkepsbndin !< ubnds inflow: 0=uniform U1, 1 = log U1, 2 = log U1 and k-eps accordingly
   integer :: jamodelspecific = 0 !< override for above two parameters

   integer :: limtypsa !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor stof transport
   integer :: limtypTM !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor stof transport
   integer :: limtypsed !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor stof transport
   integer :: limtyphu !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor hu
   integer :: limtypmom !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor momentum transport
   integer :: jalimnor !< 0=limit x/y components, 1=limit normal/tangetial components
   integer :: limtypw !< 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC, 21=central voor wave action transport

   integer :: ifixedweirscheme !< 0 = no, 1 = compact stencil, 2 = whole tile lifted, full subgrid weir + factor
   integer :: ifixedweirscheme1D2D !< 0 = use regular fixedweirscheme also on 1D2D links, 1 = iterative 1d2d lateral coupling on 1D2D links
   integer :: ifxedweirfrictscheme !< 0 = friction based on hu, 1 = friction based on subgrid weirfriction scheme
   integer :: jasetadjacentbobs = 0 !< also lift adjacent bobs and bl of kadecel
   real(kind=dp) :: fixedweircontraction !< flow width = flow width*fixedweircontraction
   real(kind=dp) :: fixedweirtopwidth !< , e.g. 4.00 (m)
   real(kind=dp) :: fixedweirtopfrictcoef !< if .ne. dmiss, use this friction coefficient on top width
   real(kind=dp) :: fixedweirtalud !< , e.g. 4 ( ) for 1 to 4 talud
   real(kind=dp) :: waquaweirthetaw = 0.6d0 !< , e.g. 0.6
   real(kind=dp) :: huweirregular = 0.0d0 !< For Tabellenboek and Villemonte:
   !< hu <  huweirregular : true flow area
   !< hu >= huweirregular : flow area as if no weir present
   real(kind=dp) :: sini !< uniform initial waterlevel (m),     (uniform bottom level = zkuni)
   real(kind=dp) :: waterdepthini1D !< uniform initial depth (m)
   real(kind=dp) :: uini !< uniform initial velociy    (m/s),
   real(kind=dp) :: salini !< uniform initial sal        (ppt)
   real(kind=dp) :: deltasalinity = -999d0 !< uniform initial sal        (ppt)
   real(kind=dp) :: Sal0abovezlev !< sal=0 above lev= zlev      (m)
   real(kind=dp) :: temini !< uniform initial temp       (degC)
   real(kind=dp) :: spirini !< uniform initial spirint    (m/s)

   real(kind=dp) :: zbnd !< for now only, uniform waterlevel on boundary
   real(kind=dp) :: zkdropstep !< Amount of bottomlevel to be added with dropland (m)
   real(kind=dp) :: sdropstep !< Amount of water to be added with dropwater (m)

   real(kind=dp), parameter :: eps4 = 1d-4 !< min au in poshchk
   real(kind=dp), parameter :: eps6 = 1d-6 !<
   real(kind=dp), parameter :: eps8 = 1d-8 !< implicit diffusion
   real(kind=dp), parameter :: eps10 = 1d-10 !<
   real(kind=dp), parameter :: eps20 = 1d-20 !< faclax
   real(kind=dp) :: epshsdif = 1d-2 !< hs < epshsdif: no vertical diffusion if hs < epshsdif
   real(kind=dp) :: s01max !< water level threshold (m) between s0 and s1 in validation routine
   real(kind=dp) :: u01max !< velocity threshold (m/s) between u0 and u1 in validation routine
   real(kind=dp) :: umagmax !< velocity threshold (m/s) for velocity magnitude in validation routine
   real(kind=dp) :: s01warn !< warning level water level (m) between s0 in validation routine
   real(kind=dp) :: u01warn !< warning level velocity (m/s) between u0 in validation routine
   real(kind=dp) :: umagwarn !< warning level velocity (m/s) for velocity magnitude in validation routine
   real(kind=dp) :: sscmax !< error level concentration (kg/m3) for velocity magnitude in validation routine
   ! See also m_flowtimes::dtminbreak

   ! parameters controlling flooding/drying/solving
   integer :: testdryflood !< Flag for testing alternative drying flooding algoritm; 0 = standard, 1 =Delft3D-FLOW
   integer :: testfixedweirs !< Flag for fixed weir options; 0 = original Villemonte approach, 1 = Sieben2007
   real(kind=dp) :: epshu !< minimum waterdepth for setting hu>0
   real(kind=dp) :: epshs !< minimum waterdepth for setting cfu
   real(kind=dp) :: epsz0 !< minimum value for roughness height
   real(kind=dp) :: chkhuexpl !< only for step_explicit:  check computed flux beneath this waterdepth
   real(kind=dp) :: chkadvd !< check advection  for 'drying' below this (upwind) waterdepth
   real(kind=dp) :: chkdifd !< check diffusion, only for jatransportautotimestepdiff== 1
   real(kind=dp) :: chkwndd !< check windstress for 'drying' below this waterdepth
   real(kind=dp) :: chktempdep !< check heatfluxes for 'drying' below this waterdepth
   real(kind=dp) :: trsh_u1Lb = 0.0d0
   integer :: jposhchk !< check for positive waterdepth; 0 = no
                                                        !!                               -1 = 1.0*dts, only check for dry cells and report back, restart Nested Newton, not timestep.
                                                        !!                                1 = 0.7*dts, just redo
                                                        !!                                2 = 1.0*dts, close all links
                                                        !!                                3 = 0.7*dts, close all links
                                                        !!                                4 = 1.0*dts, reduce au
                                                        !!                                5 = 0.7*dts, reduce au
   integer :: jsolpos !< in iterative solver force solution above bottom level
   integer :: Icgsolver !< 'Solver type , 1 = sobekGS_OMP, 2 = sobekGS_OMPthreadsafe, 3 = sobekGS, 4 = sobekGS + Saadilud, 5 = parallel/global Saad, 6 = parallel/Petsc, 7 = parallel/GS '
   integer :: ipre !< Preconditioner, 0=rowscaling, 1=GS, 2=trial
   integer :: Noderivedtypes !< 0=use derived types in gauss and substi, 5=use simple Fortran arrays (faster)
   integer :: jacheckmatrix !< checkmatrix

   integer :: mdump ! dump file unit nr

   real(kind=dp) :: hwetbed !< for case wetbed

   integer :: javau !< vert. adv. u1   : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL

   integer :: javau3onbnd = 0 !< vert. adv. u1 bnd UpwimpL: 0=follow javau , 1 = on bnd, 2= on and near bnd

   integer :: javakeps !< vert. adv. keps : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL

   integer :: javasal !< vert. adv. sa1  : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, switched to 3 for neg. strat.

   integer :: javatem !< vert. adv. tem1 : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, switched to 3 for neg. strat.

   integer :: javased !< vert. adv. suspended sediment concentrations : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=switched to 3 for neg stratif., 6=higher-order upwind/explicit

   integer :: jaimplicitfallvelocity = 1 !< fallvelocity implicit 1=yes, 0=no

   integer :: jahazlayer !< vertical treatment of horizontal advection in z layers 1=org, 2=sigma, 3=node volumes

   integer :: jaZlayeratubybob = 0 !< 0 = BL left/right based, 1: Linkbob based

   integer :: jaZlayercenterbedvel = 1 !< In z-layer model copy lowest u-velocity to lower cell centres

   integer :: jastructurelayersactive = 0 !< 0=general structure over all layers , 1=only through open layers

   integer :: jaZerozbndinflowadvection = 0 !< set zero advection velocity on inflow at z boundaries 0=no, 1=yes

   integer :: flow_solver !< 1=stepreduce, 2=1d implicit
   character(len=28) :: md_flow_solver = 'generic1d2d3d'
   integer, parameter :: FLOW_SOLVER_FM = 1
   integer, parameter :: FLOW_SOLVER_SRE = 2

   integer :: jatransportautotimestepdiff = 0 ! Auto Timestep in Transport module, 0 = limitation of diffusion, but no limitation of time-step due to diffusion, 1 = no limitation of diffusion, but limitation of time step due to diffusion, 2: no limitation of diffusion and no limitation of time step due to diffusion

   integer :: implicitdiffusion2D = 0 ! Auto Timestep in Transport module, 0 = limitation of diffusion, but no limitation of time-step due to diffusion, 1 = no limitation of diffusion, but limitation of time step due to diffusion, 2: no limitation of diffusion and no limitation of time step due to diffusion

   integer :: jadiagnostictransport = 0 ! Switch for diagnostic ("frozen") transport, 0 = prognostic transport, 1 = diagnostic transport

   integer :: jaexplicitsinks = 1

   integer :: jaanalytic !< analytic solution available in black sideview => do not also show computed surface in black

   integer :: jaustarint !< 1=integral bed layer velocity,  0=velocity at half bed layer

   integer :: jaPure1D !< along 1D channels: 0 = 2D mom Perot, 1 = 1D mom Perot using vol1_f, 2 = 1D mom Perot using vol1, 3,4,5,6,7 = 1D mom on links (2 + Iadvec1D of SOBEK)

   integer :: jaJunction1D !< at 1D junctions: 0 = 2D mom Perot, 1 = same as along the 1D channels

   real(kind=dp) :: Eddyviscositybedfacmax !< eddyviscosityatbed = min(eddyviscosityatbed, eddyviscosityatbedfacmax*eddyviscosityatbed+1 )

   real(kind=dp) :: Eddyviscositysurfacmax !< eddyviscosityatbed = min(eddyviscosityatsur, eddyviscosityatseufacmax*eddyviscosityatsur-1 )

   integer :: jaqaisq1 = 0 !< 1 : qa = q1, 0 : qa = au*u1

   integer :: inisal2D !< 1 if specified through meteo module

   integer :: initem2D !< 1 if specified through meteo module

   integer :: inised2D !< 1 if specified through meteo module

   integer :: inivel !< initial velocity (1) or not (0)

   real(kind=dp) :: cffacver = 0d0 !< switch to low order at high cf in constituent transport vertical, 1d0=yes, 0d0 = no

   real(kind=dp) :: cffachormom = 1d0 !< switch to low order at high cf in horizontal mom. transport, 1d0=yes, 0d0 = no

   real(kind=dp) :: cfexphormom = 1d0 !< exponent of same

   real(kind=dp) :: cfconhormom = 0d0 !< constant of same

   real(kind=dp) :: cffachu = 1d0 !< switch to low order at high cf in sethu, 1d0=yes, 0d0 = no

   real(kind=dp) :: cfexphu = 1d0 !< exponent of same

   real(kind=dp) :: toplayminthick !< minimum top layer thickness (m)

   real(kind=dp) :: botlayminthick !< minimum bot layer thickness (m)

   real(kind=dp) :: uniformsalinityabovez = -999d0 !< above this level uniform inisaltop (m) dmiss==do not use

   real(kind=dp) :: uniformsalinitybelowz = -999d0 !< below this level uniform inisal    (m) dmiss==do not use

   integer :: jbasqbnddownwindhs !< 0 : original hu on qbnd, 1 = downwind hs on qbnd

   integer :: maxitverticalforestersal !< 100, max iterations vertical forester

   integer :: maxitverticalforestertem !< 100, max iterations vertical forester

   real(kind=dp) :: salmax !< filter if sal > maxsal

   integer :: jaupwindsrc !< 1st-order upwind advection (1) or higher-order (0)

   integer :: jadiffusiononbnd = 1 !< 0 switches off diffusion on open boundaries

   integer :: jajre !< 0: default, 1: sb

   integer :: jasedtrails !< sedtrails custom averaged output - 0: no (default) ; 1: yes

   integer :: jasourcesink !< 1: source+sink 2:source 3:sink for sediment

   integer :: jalogsolverconvergence !< log solver convergence message bloat - 0: no (default) ; 1: yes
   integer :: jalogtransportsolverlimiting !< log transport solver limiting message bloat - 0: no (default) ; 1: yes

   integer :: jadpuopt !< option for bed level at velocity point in case of tile approach bed level: 1 = max (default). This is equivalent to min in Delft3D 4; 2 = mean.
   integer :: jaextrapbl !< option for extrapolating bed level at boundaries according to the slope: 0 = no extrapolation (default); 1 = extrapolate. Necessary for analytical solutions.

   ! written to his file yes or no
   integer :: jahisbal !< Write mass balance/volume totals to his file, 0: no, 1: yes
   integer :: jahissourcesink !< Write discharge/volume at sources/sinks, 0: no, 1: yest
   integer :: jahistur !< Write k, eps and vicww to his file, 0: no, 1: yes
   integer :: jahiswind !< Write wind velocities to his file, 0: no, 1: yes
   integer :: jahisrain !< Write precipitation intensity  (depth per time) to this file, 0: no, 1: yes
   integer :: jahisinfilt !< Write infiltration rate to this file, 0: no, 1: yes
   integer :: jahistem !< Write temperature to his file, 0: no, 1: yes
   integer :: jahisheatflux !< Write heatfluxes to his file, 0: no, 1: yes
   integer :: jahissal !< Write salinity to his file, 0: no, 1: yes
   integer :: jahisrho !< Write density  to his file, 0: no, 1: yes
   integer :: jahis_airdensity !< Write air density  to his file, 0: no, 1: yes
   integer :: jahiswatlev !< Write water level to his file, 0: no, 1: yes
   integer :: jahisbedlev !< Write bed level to his file, 0: no, 1: yes
   integer :: jahiswatdep !< Write waterd epth to his file, 0: no, 1: yes
   integer :: jahisvelvec !< Write velocity vectors to his file, 0: no, 1: yes
   integer :: jahisww !< Write upward velocity to his file, 0: no, 1: yes
   integer :: jahissed !< Write sediment transport to his file, 0: no, 1: yes
   integer :: jahiszcor !< Write the vertical coordinate to his file, 0: no, 1: yes
   integer :: jahiswav !< Write wave data to his file, 0: no, 1: yes
   integer :: jahislateral !< Write lateral data to his file, 0: no, 1: yes
   integer :: jahistaucurrent !< Write bed shear stress to his file, 0: no, 1: yes
   integer :: jahisvelocity !< Write velocity magnitude to his file, 0: no, 1: yes
   integer :: jahisdischarge !< Write discharge magnitude to his file, 0: no, 1: yes
   integer :: jahisrunupgauge !< Write runupgauge       to his file, 0: no, 1: yes
   integer :: jahiswqbot !< Write wqbot to his file, 0: no, 1: yes
   integer :: jahiswqbot3d !< Write wqbot3d to his file, 0: no, 1: yes
   integer :: jahistracers !< Write tracers          to his file, 0: no, 1: yes
   integer :: jahiscrs_flow !< Write crs_flow         to his file, 0: no, 1: yes
   integer :: jahiscrs_constituents !< Write crs_constituents to his file, 0: no, 1: yes
   integer :: jahiscrs_sediment !< Write crs_sediment     to his file, 0: no, 1: yes
   integer :: jahisdred !< Write dred             to his file, 0: no, 1: yes
   integer :: jahiswaq !< Write Water Quality    to his file, 0: no, 1: yes
   ! His output structure keywords
   integer :: jahiscgen !< Write structure parameters to his file, 0: n0, 1: yes
   integer :: jahispump !< Write pump      parameters to his file, 0: n0, 1: yes
   integer :: jahisgate !< Write gate      parameters to his file, 0: n0, 1: yes
   integer :: jahiscdam !< Write dam       parameters to his file, 0: n0, 1: yes
   integer :: jahisweir !< Write weir      parameters to his file, 0: n0, 1: yes
   integer :: jahisdambreak !< Write dambreak  parameters to his file, 0: n0, 1: yes
   integer :: jahisorif !< Write orifice   parameters to his file, 0: no, 1: yes
   integer :: jahisbridge !< Write bridge    parameters to his file, 0: no, 1: yes
   integer :: jahisculv !< Write culvert   parameters to his file, 0: no, 1: yes
   integer :: jahisuniweir !< Write univeral weir parameters to his file, 0: no, 1: yes
   integer :: jahiscmpstru !< Write compound structure parameters to his file, 0: no, 1: yes
   integer :: jahislongculv !< Write long culverts parameters to his file, 0: no, 1:yes

   ! written to map file yes or no
   integer :: jamaps0 !< previous step water levels to map file, 0: no, 1: yes
   integer :: jamaps1 !< water levels to map file, 0: no, 1: yes
   integer :: jamapevap !< evaporation to map file, 0: no, 1: yes
   integer :: jamapvol1 !< Volumes to map file, 0: no, 1: yes
   integer :: jamaphs !< Water depths to map file, 0: no, 1: yes
   integer :: jamaphu !< Water depths on u point to map file, 0: no, 1: yes
   integer :: jamapanc !< Ancillary variables attribute added to map file, 0: no, 1: yes (http://cfconventions.org/cf-conventions/v1.6.0/cf-conventions.html#ancillary-data)
   integer :: jamapau !< Normal flow areas au to map file, 0: no, 1: yes
   integer :: jamapu1 !< velocities to map file, 0: no, 1: yes
   integer :: jamapu0 !< previous step velocities to map file, 0: no, 1: yes
   integer :: jamapucvec !< velocity vectors to map file, 0: no, 1: yes
   integer :: jamapucmag !< velocity vector magnitude to map file, 0: no, 1: yes
   integer :: jamapucqvec !< velocity vectors (discharge based) to map file, 0: no, 1: yes
   integer :: jamapww1 !< upward velocity on flow link to map file, 0: no, 1: yes
   integer :: jamapnumlimdt !< Write the total number of times a cell was Courant limiting to map file, 0: no, 1: yes
   logical :: write_numlimdt_file !< Write the total number of times a cell was Courant limiting to <run_id>_numlimdt.xyz file
   integer :: jamaptaucurrent !< shear stress to map file, 0: no, 1: yes
   integer :: jamapz0 !< roughness heights to map file, 0: no, 1: yes
   integer :: jamap_chezy_elements !< chezy roughness in flow elements to map file, 0: no, 1: yes
   integer :: jamap_chezy_links !< chezy roughness on flow links to map file, 0: no, 1: yes
   integer :: jamap_chezy_input !< chezy input roughness on flow links to map file, 0: no, 1: yes
   integer :: jamapsal !< salinity to map file, 0: no, 1: yes
   integer :: jamaptem !< temperature to map file, 0: no, 1: yes
   integer :: jamapcali !< roughness calibration factors to map file, 0: no, 1: yes
   integer :: jamapconst !< constituents to map file, 0: no, 1: yes
   integer :: jamapsed !< sediment fractions to map file, 0: no, 1: yes
   integer :: jamaptur !< k, eps and vicww to map file, 0: no, 1: yes
   integer :: jamaptrachy !< trachytope roughnesses to map file, 0: no, 1: yes
   integer :: jamaprain !< wind velocities to map file, 0: no, 1: yes
   integer :: jamapicept !< Interception layer to map file, 0: no, 1: yes
   integer :: jamapwind !< wind velocities to map file, 0: no, 1: yes
   integer :: jamapwindstress !< wind stress to map file, 0: no, 1: yes
   integer :: jamap_airdensity !< air density to mao file, 0: no, 1: yes
   integer :: jamapviu !< horizontal viscosity to map file, 0: no, 1: yes
   integer :: jamapdiu !< horizontal diffusity to map file, 0: no, 1: yes
   integer :: jamaprho !< flow density to map file, 0: no, 1: yes
   integer :: jamapq1 !< flow flux to map file, 0: no, 1: yes
   integer :: jamapq1main !< main channel flow flux to map file, 0: no, 1: yes
   integer :: jamapfw !< fixed weir energy loss to map file, 0: no, 1: yes
   integer :: jamapspir !< spiral flow to map file, 0: no, 1: yes
   integer :: jamaptidep !< tidal potential to map file, 0: no, 1: yes
   integer :: jamapselfal !< self attraction and loading potential to map file, 0: no, 1: yes
   integer :: jamapIntTidesDiss !< internal tides dissipation to map file, 0: no, 1: yes
   integer :: jamapNudge !< output nudging to map file, 0: no, 1: yes
   integer :: jamapPure1D_debug !< additional Pure1D debugging output to map file, 0: no, 1: yes
   integer :: jamapwav !< output waves to map file, 0: no, 1: yes
   integer :: jamapwav_hwav !< output waves to map file for variable hwav,   0: no, 1: yes
   integer :: jamapwav_twav !< output waves to map file for variable twav,   0: no, 1: yes
   integer :: jamapwav_phiwav !< output waves to map file for variable phiwav, 0: no, 1: yes
   integer :: jamapdtcell !< output time steps per cell based on CFL
   integer :: jamapTimeWetOnGround !< output to map file the cumulative time when water is above ground level, 0: no, 1: yes
   integer :: jamapFreeboard !< output freeboard to map file, 0: no, 1: yes
   integer :: jamapDepthOnGround !< output waterdepth above ground level, 0: no, 1: yes
   integer :: jamapVolOnGround !< output volume above ground level, 0: no, 1: yes
   integer :: jamapTotalInflow1d2d !< output total 1d2d inflow to map file, 0: no, 1: yes
   integer :: jamapTotalInflowLat !< output total lateral inflow to map file, 0: no, 1: yes
   integer :: jamapS1Gradient !< output water level gradient to map file, 0: no, 1: yes
   integer :: jatekcd !< tek output with wind cd coefficients, 0=no (default), 1=yes
   integer :: jafullgridoutput !< 0: static layer positions, 1: time- and space-varying grid layer data, 2: time- and space-varying grid layer data with CF-bounds
   integer :: jaeulervel !< 0:GLM, 1:Euler velocities
   integer :: jamombal !< records some gradients of primitives 0:no, 1:yes
   integer :: jarstbnd !< Waterlevel, bedlevel and coordinates of boundaries, 0: no, 1: yes
   integer :: jamapbnd !< Includes boundary points in map output
   integer :: jamapqin !< Includes sum of all influxes in map output
   integer :: jaeverydt !< Write output to map file every dt, based on start and stop from MapInterval, 0=no (default), 1=yes
   integer :: jamapFlowAnalysis !< Write flow analysis output to map file
   integer :: jamapNearField !< Nearfield related output
   integer :: jamapice !< Ice cover related output
   integer :: jamapwqbot3d !< Write wqbot3d to map file, 0: no, 1: yes

! read from restart
   integer :: jarstignorebl !< Flag indicating if bed level on restart file should be ignored (0/1, default: 0)

! Write partition domain file
   integer :: japartdomain !< Write a separate netcdf file for partition domain info., 0: no, 1: yes

   real(kind=dp) :: epswetout !< Waterdepth threshold, above which a cell counts as 'wet'. For output purposes.

! Write shape files
   integer :: jashp_crs !< Write a shape file for cross sections
   integer :: jashp_obs !< Write a shape file for observation points
   integer :: jashp_weir !< Write a shape file for weirs
   integer :: jashp_thd !< Write a shape file for thin dams
   integer :: jashp_gate !< Write a shape file for gates
   integer :: jashp_emb !< Write a shape file for Embankments
   integer :: jashp_fxw !< Write a shape file for fixed weirs
   integer :: jashp_src !< Write a shape file for source-sinks
   integer :: jashp_pump !< Write a shape file for pumps
   integer :: jashp_dry !< Write a shape file for dry areas
   integer :: jashp_genstruc !< Write a shape file for general structures
   integer :: jashp_dambreak !< Write a shape file for dam breaks

   integer :: jambawritetxt !< Option to write areas mass balance terms to a txt-file
   integer :: jambawritecsv !< Option to write areas mass balance terms to a csv-file
   integer :: jambawritenetcdf !< Option to write areas mass balance terms to a netCDF-file

   integer :: jambalumpmba !< Lump MBA from/to other areas mass balance terms
   integer :: jambalumpbnd !< Lump MBA boundary mass balance terms
   integer :: jambalumpsrc !< Lump MBA source/sink mass balance terms
   integer :: jambalumpproc !< Lump MBA processes mass balance terms

   integer :: jawriteDFMinterpretedvalues !< Write interpretedvalues
   integer :: jawriteDetailedTimers !< Write detailed timers output file

! parameters for parms solver
   integer, parameter :: NPARMS_INT = 2 !< for parms solver, number of integer parameters
   integer, parameter :: IPARMS_ILUTYPE = 1
   integer, parameter :: IPARMS_NLEVEL = 2
   character(len=128), dimension(NPARMS_INT), parameter :: iparmsnam = [character(len=128) :: 'ilutype', 'nlevel']
   integer, dimension(NPARMS_INT) :: iparms

   integer, parameter :: NPARMS_DBL = 1 !< for parms solver, number of real(kind=dp) parameters
   integer, parameter :: IPARMS_DTOL = 1
   character(len=128), dimension(NPARMS_DBL), parameter :: dparmsnam = [character(len=128) :: 'dtol']
   real(kind=dp), dimension(NPARMS_DBL) :: dparms

! parameters for nudging
   real(kind=dp) :: Tnudgeuni = 3600d0 !< uniform nudge relaxation time

! parameters for internal tides dissipation
   real(kind=dp) :: ITcap !< limit to Internal Tides Dissipation / area (J/(m^2 s))

! Advection modelling at barriers
   integer :: jabarrieradvection = 1

   ! parameter for bed roughness and transport
   integer :: v2dwbl

   ! parameter for secondary flow
   integer :: ispirparopt ! for visualization

   integer, parameter :: PEROT_STATIC = 0 ! Initialise Perot weights once
   integer, parameter :: PEROT_UPDATE = 1 ! Initialise Perot weights every time-step

contains
!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_flowparameters() instead.
   subroutine default_flowparameters()
      itstep = 2 ! time step 0=only transport, 1=transport + velocity update, 2=full implicit step_reduce
      iadvec = 33 ! adv type, 0=no, 1= Wenneker vol, qu-udzt array, 2=1, function, 3=Perot in uit, 4=Perot in, 5=3,piaczek
      iadvec1D = 33 ! same, now for 1D links
      iadveccorr1D2D = 0 ! Advection correction of 1D2D link volume (0: none, 1: link volume au*dx')

      maxNonlinearIterations = 100 !< maximal iterations in non linear iteration loop before a time step reduction is applied
      setHorizontalBobsFor1d2d = .false. !< bobs are set to 2d bedlevel, to prevent incorrect storage in sewer system.
      dxDoubleAt1DEndNodes = .true. !< indicates whether a 1D grid cell at the end of a network has to be extended with 0.5*dx
      changeVelocityAtStructures = .false. !< Set the flow velocity at structures in setucxucyucxuucyu to the
      !< flow velocity upstream of the structure
      changeStructureDimensions = .true. !< Change the crestwidth of a structure, in case the crestwidth is larger than the
      !< wet surface width and make sure the crest level is equal or larger than the
      !< bed level of the channel.
      lincontin = 0 ! 0 = no, 1 = yes linear continuity

      Perot_type = PEROT_AREA_BASED ! Perot weighting type of cell center velocities ucx, ucy

      Perot_weight_update = PEROT_STATIC ! update Perot weights for 1D nodes (0: no (default), 1: yes)

      icorio = 5 ! Coriolis weigthing
      ! (Tx,Ty) = tangential unit vector at u-point
      ! uk      = u1 at layer k,
      ! hu      = hu(2D)                                     ; huk   = hu(L)
      ! hs      = hs(2D)                                     ; hsk   = zws(k) - zws(k-1)
      ! ahu     = alfa(hs) = acL(LL)*hs1 + (1-acL(LL))*hs2   ; ahuk  = alfa(hsk)
      ! hus     = areaweighted( hu) at s point               ; husk  = hus(k)
      ! ahus    = areaweighted(ahu) at s point               ; ahusk = ahus(k)
      ! .       = dotp
      ! avolu   = alfa(vol1)                                 ; avoluk = alfa(vol1(k))

      ! unweighted
      ! 3 : vk = alfa LR (Tx, Ty) . (ucxk , ucyk)              ucxk, ucyk  =   Perotsum (uk), (== ucx,ucy), f node based, fcori
      ! 4 : vk = alfa LR (Tx, Ty) . (ucxk , ucyk)              ucxk, ucyk  =   Perotsum (uk), (== ucx,ucy), f link based, fcor

      ! Olga type weightings
      ! 5 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*huk) )    / hsk Olga
      ! 6 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*hu)  )    / hs

      ! 7 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*ahuk) )   / ahusk
      ! 8 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*ahu)  )   / ahus

      ! 9 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*avoluk) ) / volk
      !10 : vk = alfa LR (Tx, Ty) . (ucxqk, ucyqk)             ucxqk,ucyqk = ( Perotsum (uk*avolu)  ) / vol

      ! David type weightings
      !25 : vk = alfa LR (Tx, Ty) . (ucxk*hsk   , ucyk*hsk )   / huk
      !26 : vk = alfa LR (Tx, Ty) . (ucxk*hs    , ucyk*hs  )   / hu

      !27 : vk = alfa LR (Tx, Ty) . (ucxk*hsk   , ucyk*ahusk ) / ahuk
      !28 : vk = alfa LR (Tx, Ty) . (ucxk*hs    , ucyk*ahus  ) / ahu

      !29 : vk = alfa LR (Tx, Ty) . (ucxk*vol1k , ucyk*vol1k ) / avoluk   identical to advec33
      !30 : vk = alfa LR (Tx, Ty) . (ucxk*vol1  , ucyk*vol1  ) / avolu

      hhtrshcor = 0d0 ! 0=no safety on hu/hs in corio

      trshcorio = 1.0 ! below this depth coriolis force scaled down linearly to 0

      jatidep = 1 ! use tide potential forcing yes no

      jaselfal = 0 ! use self attraction and loading yes no
      jaSELFALcorrectWLwithIni = 0 !< correct water level with initial atmospheric pressure in SAL

      ! DOODSONSTART = 55.565D0 ; DOODSONSTOP = 375.575D0 ; Doodsoneps = .00D0    ! standaard TRIWAQ alle 484 cmp
      DOODSONSTART = 55.565d0; DOODSONSTOP = 375.575d0; Doodsoneps = .00d0 !
      ! DOODSONSTART = 57.555D0 ; DOODSONSTOP = 275.555D0 ; Doodsoneps = .03D0    ! Delft3D

      jasecflow = 0 ! include secondary flow (0=no, 1=yes)

      japillar = 0 ! include pillar (0=no, 1=yes)

      jaequili = 0 ! equilibrium secondary flow (0=no, 1=yes)

      jacreep = 0 ! Include anti-creep calculation, (0=no, 1=yes)

      jasal = 0 ! Include salinity (autoset by flow_initexternalforcings())

      jatem = 0 ! Temperature model

      janudge = 0 ! temperature and salinity nudging
      jainiwithnudge = 0 !< initialize salinity and temperature with nudge variables

      jarhoxu = 0 ! rho effects in momentum, 0=no, 1=in horizontal adv, 2=+ in vertical adv, 3 = + in pressure term

      jased = 0 ! Include sediment

      jasedtrails = 0 ! Include sedtrail averaging

      jatrt = 0 !< Include alluvial and vegetation roughness (trachytopes)

      jacali = 0 !< Include calibration factor for roughness

      jawave = 0 ! Include wave model nr

      waveforcing = 0 !< Include wave forcing

      jawavestreaming = 0 ! Switch on in D3D model: >=1 : streaming mom , >= 2 : streaming mom + turb

      jawavestokes = 1 ! Vertical Stokes profile: 0=no, 1 = uniform, 2 = second order Stokes profile

      jawavebreakerturbulence = 1 ! Add wave-induced production terms in turbulence modelling: 0 = no, 1 = yes

      jawavedelta = 1 ! Wave boundary layer formulation: 1=Sana; 2=Nguyen

      jawaveforces = 1

      jawaveSwartDelwaq = 0 !< communicate to Delwaq taucur + tauwave instead of taucur

      modind = 0 !< Nr of wave-current bed friction model, 9 = vanrijn, 1 = fredsoe, etc like d3d

      jafrculin = 0 !< do not use linear friction

      jafrcInternalTides2D = 0 !< do not use internal tides friction

      javiusp = 0 !< spatially varying eddyviscosity yes/no 1/0

      jadiusp = 0 !< spatially varying eddydiffusivity yes/no 1/0

      jaCdwusp = 0

      ja_wind_speed_factor = 0 !< use wind speed multiplication factor 1/0

      ja_solar_radiation_factor = 0 !< use solar radiation multiplication factor 1/0

      ihorvic = 0 !< 0=no visc, 1=do visc

      iuvfield = 0 ! intialise this velocityfield: 0 = no
      ! 1:u=y**2, 2:idem, 60 deg, 3:rotation, 4=lin, 5=lin 60 deg

      istresstyp = 3 ! 1 : full stress tensor, semi  link oriented horvic2
      ! 2 : full stress tensor, fully link oriented dvxc = ok and fast
      ! 3 : 2, volume weighted
      ! 4 : full node oriented
      ! 5 : 4, volume weighted

      irov = 0 ! 0 : free slip
      ! 1 : partial slip
      ! 2 : no slip
      ! 3 : glass  (mind you, in D3DFLOW 3 means free slip)

      ibedlevmode = BLMODE_DFM !< Default: Compute bed levels solely by ibedlevtyp, i.e., derived from velocity points (or direct bl tiles).

      ibedlevtyp = 3 ! 1 : Bottom levels at waterlevel cells (=flow nodes), like tiles xz, yz, bl , bob = max(bl left, bl right)
      ! 2 : Bottom levels at velocity points  (=flow links),            xu, yu, blu, bob = blu,    bl = lowest connected link
      ! 3 : Bottom levels at velocity points  (=flow links), using mean network levels xk, yk, zk  bl = lowest connected link
      ! 4 : Bottom levels at velocity points  (=flow links), using min  network levels xk, yk, zk  bl = lowest connected link
      ! 5 : Bottom levels at velocity points  (=flow links), using max  network levels xk, yk, zk  bl = lowest connected link

      blmeanbelow = -999d0
      blminabove = -999d0

      ibedlevtyp1D = 3 !< 1 : same, 1D, 1 = tiles, xz(flow)=zk(net), bob(1,2) = max(zkr,zkl) , 3=mean netnode based

      izbndpos = 0 !< 0 : waterlevel boundary location as in D3DFLOW, 1=on network boundary, 2=on specified boundary polyline
      jaupdbndbl = 1 !< Update bl at boundary (1 = update, 0 = no update)

      nonlin1D = 0 !< 1 : non-linear continuity eq, now governed by iproftyp in volsur, 0 for rectan, else 1
      nonlin2D = 0 !< 1 : non-linear continuity eq in 2D, sets nonlin

      iproftypuni = 3 !< 1 : circle, 2 : rectan R=A/P , 3 = rectan, R=H
      iproftypuni5 = -2 !< 1 : circle, 2 : rectan R=A/P , 3 = rectan, R=H
      iproftypuni7 = -2 !< 1 : circle, 2 : rectan R=A/P , 3 = rectan, R=H

      slotw2D = 0d-3
      slotw1D = 1d-3
      jafullgridoutput = 0 !output grid in compact manner or full manner
      jaeulervel = 0 !GLM velocities

      jaconveyance2D = 1 !

      bedslope = 0d0 ! bottom inclination testcases
      Slopedrop2D = 0d0 ! Apply droplosses only if local bottom slope > Slopedrop2D, negative = no droplosses
      Drop1D = .false.
      Drop2D = 0d0 ! Apply droplosses in 2D yes or no 1 or 0
      Drop3D = 1d0 ! Apply droplosses in 3D yes or no 1 or 0
      jacstbnd = 0
      jajre = 0
      jasourcesink = 1

      cflmx = 0.7d0 ! max Courant nr ()
      cflw = 0.1d0 ! wave velocity fraction, total courant vel = u + cflw*wavevelocity
      teta0 = 0.55d0 ! 1.00d0   ! .52      ! uniform teta in horizontal (),
      ivariableteta = 0 ! 0=fully implicit,   1=teta constant,        2=variable teta
      ! (set teta=1.0)      (set teta=0.51->0.99)   (set teta<0)

      jaLogprofatubndin = 1
      jaLogprofkepsbndin = 0

      limtypsa = 4 ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor scalar tr-ansport SALINITY
      limtypTM = 4 ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor scalar transport TEMPERATURE
      limtypsed = 4 ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor scalar transport SEDIMENT
      limtyphu = 0 ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor hu WATERHEIGHT AT U POINT
      limtypmom = 4 ! 0=no, 1=minmod, 2=vanleer, 3=koren 4=MC voor MOMENTUM transport
      jalimnor = 0 ! 0=limit x/y components, 1=limit normal/tangetial components
      limtypw = 4

      ifixedweirscheme = 6 !< 0 = no special treatment, setbobs only, 1 = compact stencil, 2 = whole tile lifted, full subgrid weir + factor
      ifixedweirscheme1D2D = 0 !< 0 = use regular fixedweirscheme also on 1D2D links, 1 = iterative 1d2d lateral coupling on 1D2D links
      fixedweircontraction = 1d0 !< flow width = flow width*fixedweircontraction
      fixedweirtopwidth = 3d0 !< e.g. 4.00 (m)
      fixedweirtopfrictcoef = -999d0 !< if .ne. dmiss, use this friction coefficient on top width
      fixedweirtalud = 4d0 !< e.g. 1 to 4 talud
      isimplefixedweirs = 1
      ifxedweirfrictscheme = 0 !< 0 = friction based on hu, 1 = friction based on subgrid weirfriction scheme

      iNormalMethod = 0
      jaimplicit = 0
      jafilter = 0
      filterorder = 2
      jacheckmonitor = 0

      sini = 0d0 ! uniform initial waterlevel (m),   (uniform bottom level = zkuni)
      waterdepthini1D = -999d0
      uini = 0 ! uniform initial velocity   (m/s)
      salini = 0d0 !< uniform initial sal       (ppt)
      temini = 6d0 !< uniform initial temp      (degC)
      spirini = 0d0 !< uniform initial spirint   (m/s)

      Sal0abovezlev = -999d0 !< if not dmiss, only seta salini below this level
      zkdropstep = 1d-2 !< Amount of bottomlevel to be added with dropland (m)
      sdropstep = 1d0 !< Amount of water to be added with dropwater (m)
      uniformhu = -999d0 !< Uniformhu

      zbnd = 2d0 ! for now only, uniform waterlevel on boundary

      s01max = 0d0 ! max. water level change: off
      u01max = 0d0 ! max. velocity change: off
      umagmax = 0d0 ! max. velocity: off
      ! See also: m_flowtimes::dtminbreak
      s01warn = 0d0
      u01warn = 0d0
      umagwarn = 0d0
      sscmax = 0d0

      ! parameters controlling flooding/drying/solving
      epshu = 1d-4 ! minimum waterdepth for setting hu>0
      epshs = .2d0 * epshu ! minimum waterdepth for setting cfu
      epsz0 = 1d-5 ! minimum value for z0
      chkhuexpl = 0.1d0 ! only for step_explicit:  check computed flux beneath this waterdepth
      chkadvd = 0.1d0 ! check advection  for 'drying' below this (upwind) waterdepth
      chkdifd = 0.01d0 ! check diffusion only for jatransportautotimestepdiff == 1
      chkwndd = 0.1d0 ! check windstress              below this waterdepth
      chktempdep = 0.1d0 ! check heatfluxes              below this waterdepth
      testdryflood = 0 ! test different options for drying flooding:
      !                                0 = standard D-Flow FM
      !                                1 = Delft3D-FLOW check in water level points
      !                                2 = Minimum value for VOL1 of ba*epshu in transport equation
      testfixedweirs = 0 ! test different options for fixed weirs:
      !                                0 = original Villemonte (Sieben2010) approach
      !                                1 = 1 = Sieben2007

      jposhchk = 2 ! check for positive waterdepth; 0 = no
      !                                1 = 0.7*dts, just redo
      !                                2 = 1.0*dts, close all links
      !                                3 = 0.7*dts, close all links
      !                                4 = 1.0*dts, reduce au
      !                                5 = 0.7*dts, reduce au
      jsolpos = 0 ! in iterative solver force solution above bottom level
      Icgsolver = 4 !    Icgsolver = 1      ! 1 = GS_OMP, 2 = GS_OMPthreadsafe, 3 = GS, 4 = Saadilud
      ipre = 0 ! preconditioner, 0=rowscaling, 1=GS, 2=trial
      Noderivedtypes = 5 ! 0=use derived types in gauss and substi, 5=use simple Fortran arrays (faster)

      hwetbed = 0.2d0 ! for case wetbed

      javau = 6 !< vert. adv. u1   : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=Quick, 6=centerbased upw. expl.
      javakeps = 3 !< vert. adv. keps : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL
      javasal = 6 !< vert. adv. sa1  : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=switched to 3 for neg stratif, 6=hoexplicit.
      javatem = 6 !< vert. adv. tem1 : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=switched to 3 for neg stratif.
      javased = 6 !< vert. adv. suspended sediment concentrations : 0=No, 1=UpwexpL, 2=Centralexpl, 3=UpwimpL, 4=CentraLimpL, 5=switched to 3 for neg stratif., 6=higher-order upwind/explicit
      jahazlayer = 0 !<
      jaPure1D = 0 !< 0 = org 1D advec, 1 = 1D mom Perot using vol1_f, 2 = 1D mom Perot using vol1, 3,4,5,6,7 = 1D mom on links (2 + Iadvec1D of SOBEK)
      jaJunction1D = 1 !< 0 = org 1D advec at junctions, 1 = junctions follow jaPure1D approach
      JaZlayercenterbedvel = 1
      jastructurelayersactive = 1
      JaZerozbndinflowadvection = 0
      md_flow_solver = 'generic1d2d3d'

      jaanalytic = 0 !< analytic solution available in black sideview => do not also show computed surface in black
      jaustarint = 1 !< 1=integral bed layer velocity,  0=velocity at half bed layer
      Eddyviscositybedfacmax = 0d0 !< eddyviscosityatbed = min(eddyviscosityatbed, eddyviscosityatbedfacmax*eddyviscosityatbed+1 )
      Eddyviscositysurfacmax = 0d0 !< eddyviscosityatbed = min(eddyviscosityatsur, eddyviscosityatseufacmax*eddyviscosityatsur-1 )
      inisal2D = 0 !< 1 if specified through meteo module
      initem2D = 0 !< 1 if specified through meteo module
      inised2D = 0 !< 1 if specified through meteo module
      inivel = 0 !< initial velocities prescribed (1) or not (other)

      toplayminthick = 0.01d0 ! minimum top layer thickness (m)

      jbasqbnddownwindhs = 0 !< 0 : original hu on qbnd, 1 = downwind hs on qbnd

      maxitverticalforestersal = 0 !< 100, max iterations vertical forester
      maxitverticalforestertem = 0 !< 100, max iterations vertical forester

      salmax = 0d0 !< filter if sal > maxsal
      ! Remaining of variables is handled in reset_flowparameters()
      ! call reset_flowparameters()

      iparms = 0 ! parms-default
      dparms = 0d0 ! parms-default

      jaupwindsrc = 1
      jalogsolverconvergence = 0
      jalogtransportsolverlimiting = 0

      jahisbal = 1
      jahissourcesink = 1
      jahistur = 1
      jahiswind = 1
      jahisrain = 1
      jahisinfilt = 1
      jahistem = 1
      jahisheatflux = 1
      jahissal = 1
      jahisrho = 1
      jahis_airdensity = 0
      jahiswatlev = 1
      jahisbedlev = 1
      jahiswatdep = 0
      jahisvelvec = 1
      jahisww = 0
      jahissed = 1
      jahiszcor = 1
      jahiswav = 1
      jahislateral = 1
      jahistaucurrent = 1
      jahisvelocity = 1
      jahisdischarge = 1
      jahisrunupgauge = 1
      jahiswqbot = 1
      jahiswqbot3d = 0
      jahistracers = 1
      jahiscrs_flow = 1
      jahiscrs_constituents = 1
      jahiscrs_sediment = 1
      jahisdred = 1
      jahiswaq = 1
      jamaps0 = 1
      jamaps1 = 1
      jamapevap = 0
      jamapvol1 = 0
      jamaphs = 1
      jamaphu = 0
      jamapanc = 0
      jamapau = 0
      jamapu0 = 1
      jamapu1 = 1
      jamapucvec = 1
      jamapucmag = 1
      jamapucqvec = 0
      jamapww1 = 1
      jamapnumlimdt = 1
      write_numlimdt_file = .false.
      jamaptaucurrent = 1
      jamapz0 = 0
      jamap_chezy_elements = 0
      jamap_chezy_links = 0
      jamap_chezy_input = 0
      jamapsal = 1
      jamaptem = 1
      jamapconst = 1
      jamapsed = 1
      jamaptur = 1
      jamaptrachy = 1
      jamapcali = 1
      jamaprain = 0
      jamapicept = 0
      jamapwind = 1
      jamapwindstress = 0
      jamap_airdensity = 0
      jamapviu = 1
      jamapdiu = 1
      jamaprho = 1
      jamapq1 = 1
      jamapq1main = 0
      jamapfw = 0
      jamapspir = 1
      jamaptidep = 1
      jamapselfal = 1
      jamapIntTidesDiss = 1
      jamapNudge = 1
      jamapPure1D_debug = 0
      jamapwav = 1
      jamapdtcell = 0
      jamapTimeWetOnGround = 0
      jamapFreeboard = 0
      jamapDepthOnGround = 0
      jamapVolOnGround = 0
      jamapTotalInflow1d2d = 0
      jamapTotalInflowLat = 0
      jamapS1Gradient = 0
      jamapFlowAnalysis = 0
      jamapNearField = 0
      jamapice = 0
      jamapwqbot3d = 0

      jarstignorebl = 0

      epswetout = epshs ! the same as numerical threshold to counts as 'wet'.
      jatekcd = 1 ! wind cd coeffs on tek
      jarstbnd = 1
      jamapbnd = 0
      jamapqin = 0
      jaeverydt = 0
      japartdomain = 1
      jashp_crs = 0
      jashp_obs = 0
      jashp_weir = 0
      jashp_thd = 0
      jashp_gate = 0
      jashp_emb = 0
      jashp_fxw = 0
      jashp_src = 0
      jashp_pump = 0
      jashp_dry = 0
      jashp_genstruc = 0
      jashp_dambreak = 0
      jambawritetxt = 1
      jambawritecsv = 0
      jambawritenetcdf = 0

      jambalumpmba = 0
      jambalumpbnd = 0
      jambalumpsrc = 0
      jambalumpproc = 0

      jawriteDFMinterpretedvalues = 0
      jawriteDetailedTimers = 0

      ispirparopt = 1

      Tnudgeuni = 3600d0 !< uniform nudge relaxation time (s)
      ITcap = 0d0 !< limit to Internal Tides Dissipation / area (J/(m^2 s))

      jatransportautotimestepdiff = 0
      implicitdiffusion2D = 0

      jadpuopt = 1
      jaextrapbl = 0

      call reset_flowparameters()
   end subroutine default_flowparameters

!> Resets only flowparameters variables intended for a restart of an existing flow simulation (same MDU).
!! Upon loading of new model/MDU, call default_flowparameters() instead.
   subroutine reset_flowparameters()
   end subroutine reset_flowparameters

end module m_flowparameters
