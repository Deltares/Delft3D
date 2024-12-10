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

! unstruc.f90
module fm_external_forcings_data
   use m_wind
   use m_nudge
   use m_bnd
   implicit none

   logical :: success !< want je wil maar liever succes
   integer :: jatimespace !< doen ja/nee 1/0
   integer :: mhis !< unit nr external forcings history *.exthis
   integer :: kx, filetype, mext
   character(len=256) :: qid
   character(len=1) :: operand
   integer :: numbnp !< total nr of open boundary cells for network extension
   integer :: jaoldrstfile !< using old-version rst file, which does not contain boundary info
   ! For postprocessing, each boundary polyline is named as a open boundary section.
   ! For each section, all underlying netlinks are administered, along with the name of the pli file.
   integer, parameter :: IBNDTP_UNKNOWN = -1 !< Uninitialized boundary type/fill value
   integer, parameter :: IBNDTP_CLOSED = 0 !< Closed boundary (not used yet)
   integer, parameter :: IBNDTP_ZETA = 1 !< Water level boundary
   integer, parameter :: IBNDTP_U = 2 !< Velocity boundary (not detailed in q/u/normal/.. yet)
   integer, parameter :: IBNDTP_1D2D = 3 !< Special 1D2D boundary

   integer :: nopenbndsect !< Nr. of open boundary sections.
   integer, allocatable :: openbndtype(:) !< (nopenbndsect) Type of this boundary section (one of IBNDTP_ZETA, etc...)
   integer, allocatable :: nopenbndlin(:) !< (nopenbndsect) Nr. of links/exchanges per section.
   integer, allocatable :: openbndlin(:) !< (sum(nopenbndlin(1:nopenbndsect)) Net link nrs for each open boundary.
   character(len=256), allocatable :: openbndname(:) !< (nopenbndsect)
   character(len=256), allocatable :: openbndfile(:) !< (nopenbndsect)
   real(kind=dp), allocatable :: bndWidth1D(:) !< (nopenbndsect) Optional custom width for (1D) boundary flow links.
   real(kind=dp), allocatable :: bndBlDepth(:) !< (nopenbndsect) Optional custom bed level depth for boundary flow links.
   integer, allocatable :: lnxbnd(:) !< (lnx-lnxi) Mapping from boundary flow link nr to nopenbndsect index.

   integer, parameter :: NTRANSFORMCOEF = 26
   real(kind=dp) :: transformcoef(NTRANSFORMCOEF) !< Transform coefficients a+b*x

   integer, allocatable :: kez(:) !< temp (numl) edge oriented z lev
   integer, allocatable :: keu(:) !< temp (numl) edge oriented u vel
   integer, allocatable :: kes(:) !< temp (numl) edge oriented s sal
   integer, allocatable :: ketm(:) !< temp (numl) edge oriented tm sem
   integer, allocatable :: kesd(:) !< temp (numl) edge oriented sd sed
   integer, allocatable :: ket(:) !< temp (numl) edge oriented t tang.  vel.
   integer, allocatable :: keuxy(:) !< temp (numl) edge oriented uxuy vel.
   integer, allocatable :: ken(:) !< temp (numl) edge oriented n normal vel.
   integer, allocatable, target :: ke1d2d(:) !< temp (numl) edge oriented 1d2d bnd
   integer, allocatable :: keg(:) !< temp (numl) edge oriented g gate
   integer, allocatable :: ked(:) !< temp (numl) edge oriented d cdam
   integer, allocatable :: kegen(:) !< temp (numl) edge oriented general structure
   integer, allocatable :: kegs(:) !< temp (numl) edge oriented general structure new style
   integer, allocatable :: kep(:) !< temp (numl) edge oriented p pump
   integer, allocatable :: keklep(:) !< temp (numl) edge oriented check valve
   integer, allocatable :: kevalv(:) !< temp (numl) edge oriented check valve
   integer, allocatable :: kew(:) !< temp (numl) edge oriented w waves
   integer, allocatable :: ketr(:, :) !< temp (numl) edge oriented tracer
   integer, allocatable :: kesf(:, :) !< temp (numl) edge oriented sedfrac
   integer, allocatable :: kedb(:) !< temp (numl) edge oriented dambreak

   integer, allocatable :: itpez(:) !< temp (numl) edge oriented,
                                                        !! 1,*=boundary typ, see type indicator kbndz(4,*) below
   integer, allocatable :: itpeu(:) !< hulp (numl) edge oriented,
                                                        !! 1,*=boundary typ, see type indicator kbndz(4,*) below
   integer, allocatable :: itpenz(:) !< temp (numl) edge oriented,
                                                        !! 1,*=boundary number (nopenbndsect), see type indicator kbndz(5,*) below
   integer, allocatable :: itpenu(:) !< temp (numl) edge oriented,
                                                        !! 1,*=boundary number (nopenbndsect), see type indicator kbndu(5,*) below
   real(kind=dp), allocatable :: ftpet(:)

   real(kind=dp), allocatable :: threttim(:, :) !< (NUMCONST,nopenbndsect) Thatcher-Harleman return times

   character(len=256), allocatable :: thrtq(:) !< temp array for Thatcher-Harleman return time readout, stores constituents
   real(kind=dp), allocatable :: thrtt(:) !< temp array for Thatcher-Harleman return time readout, stores return times
   integer, allocatable :: thrtn(:) !< temp array for Thatcher-Harleman return time readout, stores cell indices (first one)

   integer, target :: nzbnd !< number of waterlevel boundary segments
   integer, target :: nbndz !< waterlevel boundary points dimension
   real(kind=dp), allocatable :: xbndz(:) !< waterlevel boundary points xcor
   real(kind=dp), allocatable :: ybndz(:) !< waterlevel boundary points ycor
   real(kind=dp), allocatable, target :: zbndz(:) !< [m] waterlevel boundary points function  {"location": "edge", "shape": ["nbndz"]}
   real(kind=dp), allocatable :: zbndz0(:) !< waterlevel boundary points function
   real(kind=dp), allocatable :: xy2bndz(:, :) !< waterlevel boundary 'external tolerance point'
   integer, allocatable :: kdz(:) !< waterlevel boundary points temp array
   integer, allocatable, target :: kbndz(:, :) !< waterlevel boundary points index array
                                                        !! 1,* = index in s1 boundary point
                                                        !! 2,* = index in s1 first point on the inside
                                                        !! 3,* = index in u1 of their connecting link (always positive to the inside)
                                                        !! 4,* = type indicator :
                                                        !!                        1 = waterlevel boundary
                                                        !!                        2 = waterlevel neumann
                                                        !!                        3 = velocity   normal ingoing component
                                                        !!                        4 = velocity   flux boundary
                                                        !!                        5 = velocity   Riemann boundary
                                                        !!                        6 = waterlevel outflow
                                                        !! 5,* = member of boundary number somuch of this type
                                                        !! 6,* = riemann relaxation time for this point (s)
   real(kind=dp), allocatable :: zkbndz(:, :) !< only for jaceneqtr == 2 : left and right vertical netnode zk levels
   real(kind=dp) :: zbndzval1 = -999d0, zbndzval2 = -999d0
   integer, allocatable :: kbanz(:, :) !< ban pointer 2,*

   integer :: nubnd !< number of velocity boundary segments
   integer :: nbndu !< velocity   boundary points dimension
   real(kind=dp), allocatable :: xbndu(:) !< velocity   boundary points xcor
   real(kind=dp), allocatable :: ybndu(:) !< velocity   boundary points ycor
   real(kind=dp), allocatable, target :: zbndu(:) !< [m/s] velocity   boundary points function   {"location": "edge", "shape": ["nbndu"]}
   real(kind=dp), allocatable, target :: zbndq(:) !< [m3/s] discharge  boundary points function   {"location": "edge", "shape": ["nbndu"]}
   real(kind=dp), allocatable :: zbndu0(:) !< velocity   boundary points function in start time
   real(kind=dp), allocatable :: xy2bndu(:, :) !< velocity   boundary 'external tolerance point'
   integer, allocatable :: kdu(:) !< velocity   boundary points temp array
   integer, allocatable :: kbndu(:, :) !< velocity   boundary points index array, see lines above
   integer, allocatable :: L1qbnd(:) !< first  nbndu point in discharge bnd nqbnd
   integer, allocatable :: L2qbnd(:) !< second nbndu point in discharge bnd nqbnd
   real(kind=dp), allocatable :: at_all(:) !< "at" for all qbnd's, dim(nqbnd)
   real(kind=dp), allocatable :: at_sum(:) !< "at" for all qbnd's, summed over all domains, dim(nqbnd)
   real(kind=dp), allocatable :: wwssav_all(:, :) !< "wwav" and "ssav" for all qnbd's, dim(2,nqbnd)
   real(kind=dp), allocatable :: wwssav_sum(:, :) !< "wwav" and "ssav" for all qnbd's, summed over all domains, dim(2,nqbnd)
   integer :: japartqbnd !< one or more of the discharge boundaries is partitioned (1) or not (0)
   real(kind=dp), allocatable :: huqbnd(:) !< hu used in normalised Manning discharge boundary condition, based on average water-level
   integer :: nqbnd !<
   real(kind=dp) :: qbndhutrs = 0.1d0 !< only discharge bnd here if hu>qbndhutrs
   real(kind=dp), allocatable :: zkbndu(:, :) !< only for jaceneqtr == 2 : left and right vertical netnode zk levels
   integer, allocatable :: kbanu(:, :) !< ban pointer 2,*

   integer :: nbnds !< salinity   boundary points dimension in 1D and 2D
   real(kind=dp), allocatable :: xbnds(:) !< salinity   boundary points xcor
   real(kind=dp), allocatable :: ybnds(:) !< salinity   boundary points ycor
   real(kind=dp), allocatable, target :: zminmaxs(:) !< salinity   boundary points zmin and zmax
   real(kind=dp), allocatable, target :: sigmabnds(:) !< salinity   boundary points sigma coordinates (for now: dim = (nbnds*kmx) )
   real(kind=dp), allocatable, target :: zbnds(:) !< salinity   boundary points function
   real(kind=dp), allocatable :: xy2bnds(:, :) !< salinity   boundary 'external tolerance point'
   integer, allocatable :: kds(:) !< satinity   boundary points temp array
   integer, allocatable :: kbnds(:, :) !< salinity   boundary points index array, see lines above
   real(kind=dp), allocatable :: thtbnds(:) !< salinity Thatcher-Harleman outflow times (dim = (nbnds))
   real(kind=dp), allocatable :: thzbnds(:) !< salinity Thatcher-Harleman outflow concentrations (dim = (nbnds*kmx))

   integer :: nbndw !< wave    boundary points dimension
   real(kind=dp), allocatable :: xbndw(:) !< wave    boundary points xcor
   real(kind=dp), allocatable :: ybndw(:) !< wave    boundary points ycor
   real(kind=dp), allocatable :: zbndw(:, :) !< wave    boundary points function
   real(kind=dp), allocatable :: xy2bndw(:, :) !< wave    boundary 'external tolerance point'
   integer, allocatable :: kdw(:) !< wave    boundary points temp array
   integer, allocatable :: kbndw(:, :) !< wave    boundary points index array, see lines above
   integer, allocatable :: L1wbnd(:) !< first  nbndw point in wave-energy bnd nwbnd
   integer, allocatable :: L2wbnd(:) !< second nbndw point in wave-energy bnd nwbnd

   integer :: nbndtm !< temperature boundary points dimension
   real(kind=dp), allocatable :: xbndtm(:) !< temperature boundary points xcor
   real(kind=dp), allocatable :: ybndtm(:) !< temperature boundary points ycor
   real(kind=dp), allocatable, target :: zminmaxTM(:) !< temperature boundary points zmin and zmax
   real(kind=dp), allocatable, target :: sigmabndTM(:) !< temperature boundary points sigma coordinates
   real(kind=dp), allocatable, target :: zbndtm(:) !< temperature boundary points function
   real(kind=dp), allocatable :: xy2bndtm(:, :) !< temperature external tolerance point'
   integer, allocatable :: kdtm(:) !< temperature boundary points temp array
   integer, allocatable :: kbndtm(:, :) !< temperature boundary points index array, see lines above
   real(kind=dp), allocatable :: thtbndtm(:) !< temperature Thatcher-Harleman outflow times
   real(kind=dp), allocatable :: thzbndtm(:) !< temperature Thatcher-Harleman outflow concentrations

   integer :: nbndsd !< sediment   boundary points dimension
   real(kind=dp), allocatable :: xbndsd(:) !< sediment   boundary points xcor
   real(kind=dp), allocatable :: ybndsd(:) !< sediment   boundary points ycor
   real(kind=dp), allocatable, target :: zminmaxsd(:) !< sediment   boundary points zmin and zmax
   real(kind=dp), allocatable, target :: sigmabndsd(:) !< sediment   boundary points sigma coordinates
   real(kind=dp), allocatable, target :: zbndsd(:) !< sediment   boundary points function
   real(kind=dp), allocatable :: xy2bndsd(:, :) !< sediment   boundary 'external tolerance point'
   integer, allocatable :: kdsd(:) !< sediment   boundary points temp array
   integer, allocatable :: kbndsd(:, :) !< sediment   boundary points index array, see lines above
   real(kind=dp), allocatable :: thtbndsd(:) !< sediment Thatcher-Harleman outflow times
   real(kind=dp), allocatable :: thzbndsd(:) !< sediment Thatcher-Harleman outflow concentrations

   integer, allocatable :: nbndtr(:) !< tracer boundary points dimension
   integer :: nbndtr_all !< all tracer boundary points dimension (max(nbndtr))
   integer :: numtracers !< number of tracers with boundary conditions
   integer, parameter :: NAMTRACLEN = 128
   character(len=NAMTRACLEN), allocatable :: trnames(:) !< tracer names (boundary conditions only, used for look-up)
   character(len=NAMTRACLEN), allocatable :: trunits(:) !< tracer units
   type(bndtype), allocatable, target :: bndtr(:)
   real(kind=dp), allocatable :: wstracers(:) !< tracer fall velocity pos is downward (m/s)
   real(kind=dp), allocatable :: decaytimetracers(:) !< tracer decaytimes (s)
   integer :: jadecaytracers = 0 !< 0 = no, 1 =yes

   ! JRE sedfracbnds
   integer, allocatable :: nbndsf(:) !< sedfrac   boundary points dimension
   integer :: nbndsf_all !< all sedfrac boundary points dimension (max(nbndsf))
   integer :: numfracs !< number of fractions with boundary conditions
   integer, parameter :: NAMSFLEN = 128
   character(len=NAMSFLEN), allocatable :: sfnames(:) !< sedfrac names (boundary conditions only, used for look-up)
   type(bndtype), allocatable, target :: bndsf(:)
   !\ sedfracbnds

   integer :: nbndt !< tang.velocity boundary points dimension
   real(kind=dp), allocatable :: xbndt(:) !< tang.velocity boundary points xcor
   real(kind=dp), allocatable :: ybndt(:) !< tang.velocity boundary points ycor
   real(kind=dp), allocatable, target :: zbndt(:) !< tang.velocity boundary points function
   real(kind=dp), allocatable :: xy2bndt(:, :) !< tang.velocity boundary 'external tolerance point'
   integer, allocatable :: kdt(:) !< tang.velocity boundary points temp array
   integer, allocatable :: kbndt(:, :) !< tang.velocity boundary points index array, see lines above

   integer :: nbnduxy !< uxuyadvectionvelocity boundary points dimension
   real(kind=dp), allocatable :: xbnduxy(:) !< uxuyadvectionvelocity boundary points xcor
   real(kind=dp), allocatable :: ybnduxy(:) !< uxuyadvectionvelocity boundary points ycor
   real(kind=dp), allocatable, target :: zminmaxuxy(:) !< uxuyadvectionvelocity boundary points zmin and zmax
   real(kind=dp), allocatable, target :: sigmabnduxy(:) !< uxuyadvectionvelocity boundary points sigma coordinates (for now: dim = (nbnds*kmx) )
   real(kind=dp), allocatable, target :: zbnduxy(:) !< uxuyadvectionvelocity boundary points function
   real(kind=dp), allocatable :: xy2bnduxy(:, :) !< uxuyadvectionvelocity boundary 'external tolerance point'
   integer, allocatable :: kduxy(:) !< uxuyadvectionvelocity boundary points temp array
   integer, allocatable :: kbnduxy(:, :) !< uxuyadvectionvelocity boundary points index array, see lines above
   real(kind=dp) :: zbnduxyval = -999d0

   integer :: nbndn !< norm.velocity boundary points dimension
   real(kind=dp), allocatable :: xbndn(:) !< norm.velocity boundary points xcor
   real(kind=dp), allocatable :: ybndn(:) !< norm.velocity boundary points ycor
   real(kind=dp), allocatable, target :: zminmaxu(:) !< norm.velocity boundary points zmin and zmax
   real(kind=dp), allocatable, target :: sigmabndu(:) !< norm.velocity boundary points sigma coordinates (for now: dim = (nbndn*kmx) )
   real(kind=dp), allocatable, target :: zbndn(:) !< norm.velocity boundary points function
   real(kind=dp), allocatable :: xy2bndn(:, :) !< norm.velocity boundary 'external tolerance point'
   integer, allocatable :: kdn(:) !< norm.velocity boundary points temp array
   integer, allocatable :: kbndn(:, :) !< norm.velocity boundary points index array, see lines above

   integer :: ndxbnd_own !< boundary waterlevel points (without ghost points) dimension
   integer, allocatable :: ibnd_own(:) !< Index mapping own boundary points (without ghost points) to the index in all boundary points

   integer :: ngate !< gates links dimension, to specify gate lower edge level
   real(kind=dp), allocatable :: xgate(:) !< gates links xcor = xz(k1)
   real(kind=dp), allocatable :: ygate(:) !< gates links ycor
   real(kind=dp), allocatable, target :: zgate(:) !< gates lower_edge_level value
   real(kind=dp), allocatable :: xy2gate(:, :) !< gates links second point xcor = xz(k2)
   integer, allocatable :: kgate(:, :) !< gates links index array, see lines above
   integer, allocatable, target :: kdg(:) !< helper for multiple_uni
   integer, allocatable :: L1gatesg(:) !< first  ngate point in gate signal ngatesg
   integer, allocatable :: L2gatesg(:) !< second ngate point in gate signal ngatesg
   integer :: ngatesg !< nr of gate signals specified
   character(len=128), allocatable, target :: gate_ids(:)

   integer :: ncdam !< nr of controllable dam points
   real(kind=dp), allocatable :: xcdam(:) !< dam nodes xcor = xz(k1)
   real(kind=dp), allocatable :: ycdam(:) !< dam nodes ycor
   real(kind=dp), allocatable, target :: zcdam(:) !< dam nodes zvalue {"shape": ["ncdam"]}
   real(kind=dp), allocatable :: xy2cdam(:, :) !< cdams links second point xcor = xz(k2)
   integer, allocatable :: kcdam(:, :) !< cdams links index array, see lines above
   integer, allocatable, target :: kdd(:) !< helper for multiple_uni_damlevel
   integer, allocatable :: L1cdamsg(:) !< first  ncdam point in cdam signal ncdamsg
   integer, allocatable :: L2cdamsg(:) !< second ncdam point in cdam signal ncdamsg
   integer :: ncdamsg !< nr of cdam signals specified
   character(len=128), allocatable, target :: cdam_ids(:)

   integer, allocatable :: kdryarea(:) !< dry area net links index array
   integer :: nDryLinks !< number of net linls of dry are

   type pillar_type
      integer :: np !< number of pillars
      real(kind=dp), dimension(:), allocatable :: xcor !< x-coordinates of pillars
      real(kind=dp), dimension(:), allocatable :: ycor !< y-coordinates of pillars
      real(kind=dp), dimension(:), allocatable :: dia !< radius od pillars
      real(kind=dp), dimension(:), allocatable :: cd !< Cd coefficient of pillars
   end type pillar_type
   type(pillar_Type), dimension(:), allocatable :: pillar
   real(kind=dp), dimension(:), allocatable :: Cpil

   integer :: ncgen !< nr of controllable generalstr points
   real(kind=dp), allocatable :: xcgen(:) !< generalstr nodes xcor = xz(k1)
   real(kind=dp), allocatable :: ycgen(:) !< generalstr nodes ycor
   real(kind=dp), allocatable, target :: zcgen(:) !< generalstr nodes zvalue (kx=3)
   real(kind=dp), allocatable :: xy2cgen(:, :) !< cgen links second point xcor = xz(k2)

   real(kind=dp), allocatable :: Fusav(:, :) !< only needed if gatedoorheight > 0 , dim = ncgen
   real(kind=dp), allocatable :: Rusav(:, :) !< only needed if gatedoorheight > 0
   real(kind=dp), allocatable :: Ausav(:, :) !< only needed if gatedoorheight > 0
   real(kind=dp), allocatable :: ff3(:, :) !< only needed if kmx>0 and jastructurelayersactive>0

   integer, allocatable :: kcgen(:, :) !< cgen links index array, see lines above
                                                        !! 1,* = index in s1 point "left" of genstru
                                                        !! 2,* = index in s1 point "right" of genstru
                                                        !! 3,* = index in u1 of their connecting link (may point from #2 -> #1 if flow link is in opposite direction through the genstru polyline)
                                                        !! 4,* = pointer to general structure signal nr n
   integer, allocatable, target :: kdgen(:) !< helper for multiple_uni_damlevel
   integer, allocatable :: L1cgensg(:) !< first  ncdam point in cdam signal ncdamsg
   integer, allocatable :: L2cgensg(:) !< second ncdam point in cdam signal ncdamsg
   integer :: ncgensg !< nr of cdam signals specified

   integer, parameter :: ICGENTP_WEIR = 1 !< general structure type: a weir
   integer, parameter :: ICGENTP_GATE = 2 !< general structure type: a gate
   integer, parameter :: ICGENTP_GENSTRU = 3 !< general structure type: a true general structure
   character(len=128), allocatable, target :: cgen_ids(:)
   integer, allocatable :: cgen_type(:) !< (1:ngensg) The type for each general structure, one of ICGENTP_WEIR|GATE|GENSTRU
   integer, allocatable :: cgen2str(:) !< (1:ngensg) Mapping from overall ngensg index to underlying structure index in either 1:nweirgen, 1:ngategen, or 1:ngenstru (inverse from *2cgen arrays below)

   ! The user may specify different 'gate'/'weir'/'generalstructure',
   ! and all are translated into a general structure (in computations
   ! and external forcings). To distinguish them, maintain counters for each.
   ! This should hold: ncgensg = nweirgen + ngategen + ngenstru
   integer :: nweirgen !< nr of weirs in the generalstructure set
   integer :: ngategen !< nr of gates in the generalstructure set
   integer :: ngenstru !< nr of real general structures in the generalstructure set
   integer, allocatable, target :: weir2cgen(:) !< (1:nweirgen) Mapping from weir number to underlying generalstructure number
   integer, allocatable, target :: gate2cgen(:) !< (1:ngategen) Mapping from gate number to underlying generalstructure number
   integer, allocatable, target :: genstru2cgen(:) !< (1:ngenstru) Mapping from true general structure number to underlying generalstructure number

   ! Pumps and pumps with levels
   integer :: npump !< nr of pump links
   real(kind=dp), allocatable :: xpump(:) !< pump nodes xcor = xz(k1)
   real(kind=dp), allocatable :: ypump(:) !< pump nodes ycor
   real(kind=dp), allocatable, target :: qpump(:) !< pump discharge m3/s
   real(kind=dp), allocatable :: xy2pump(:, :) !< pump links second point xcor = xz(k2)
   integer, allocatable :: kpump(:, :) !< pump links index array, see lines above
   integer, allocatable, target :: kdp(:) !< helper for multiple_uni_pump
   integer, allocatable :: L1pumpsg(:) !< first  npump point in pump signal npumpsg
   integer, allocatable :: L2pumpsg(:) !< second npump point in pump signal npumpsg
   real(kind=dp), allocatable :: pumponoff(:, :) !< 1=suct on, 2=suct off, 3=deliv on, 4=deliv off , *)
   integer :: npumpsg !< nr of pump signals specified
   integer, allocatable :: L1strucsg(:) !< first  nstru point in pump signal
   integer, allocatable :: L2strucsg(:) !< second nstru point in pump signal
   !variables for pump with levels
   ! time varying
   real(kind=dp), allocatable :: waterLevelsPumpLeft(:) !< left considering flow direction
   real(kind=dp), allocatable :: waterLevelsPumpRight(:) !< right considering flow direction
   real(kind=dp), allocatable :: pumpAveraging(:, :) !< to avoid allocations/deallocations
   ! constant in time
   integer :: nPumpsWithLevels !< nr of pump signals with levels (sobek format)
   integer, allocatable :: pumpsWithLevels(:) !< -1 = legacy, not 1 = new pump
   character(len=128), allocatable, target :: pump_ids(:) !< the pumps ids

   ! Dambreak
   !time varying
   real(kind=dp), allocatable, target :: waterLevelsDambreakUpStream(:) !< the water levels computed each time step upstream
   real(kind=dp), allocatable, target :: waterLevelsDambreakDownStream(:) !< the water levels computed each time step downstream
   real(kind=dp), allocatable, target :: breachDepthDambreak(:) !< the dambreak breach width (as a level)
   real(kind=dp), allocatable, target :: breachWidthDambreak(:) !< the dambreak breach width (as a level)
   real(kind=dp), allocatable :: normalVelocityDambreak(:) !< dambreak normal velocity
   real(kind=dp), allocatable :: dambreakAveraging(:, :) !< to avoid allocations/deallocations
   real(kind=dp), allocatable :: breachWidthDerivativeDambreak(:) !< breach width derivatives
   real(kind=dp), allocatable :: waterLevelJumpDambreak(:) !< water level jumps
   !constant in time
   real(kind=dp), allocatable :: maximumDambreakWidths(:) !< the total dambreak width (from pli file)
   real(kind=dp), allocatable :: dambreakLinksEffectiveLength(:) !< dambreak maximum flow widths
   real(kind=dp), allocatable :: dambreakLinksActualLength(:) !< dambreak actual flow widths
   integer, allocatable :: dambreaks(:) !< store the dambreaks indexes among all structures
   integer, parameter :: DBW_SYMM = 1 !< symmetrical dambreak widening (limited width in case of asymmetric starting link placement)
   integer, parameter :: DBW_PROP = 2 !< dambreak wideining proportional to left/right dam length
   integer, parameter :: DBW_SYMM_ASYMM = 3 !< symmetrical dambreak widening until left/right runs out of space then continues one sided
   integer :: dambreakWidening = DBW_SYMM_ASYMM !< method for dambreak widening
   character(len=128) :: dambreakWideningString = 'symmetric-asymmetric' !< method for dambreak widening (string for input processing)
   integer :: ndambreaklinks !< nr of dambreak links
   integer :: ndambreaksignals !< nr of dambreak signals
   integer, allocatable :: L1dambreaksg(:) !< first dambreak link for each signal
   integer, allocatable :: L2dambreaksg(:) !< second dambreak link for each signal
   integer, allocatable :: activeDambreakLinks(:) !< activeDambreakLinks, open dambreak links
   integer, allocatable :: LStartBreach(:) !< the starting link, the closest to the breach point
   integer, allocatable :: kdambreak(:, :) !< dambreak links index array
   real(kind=dp), allocatable, target :: dambreakLevelsAndWidthsFromTable(:) !< dambreak widths and heights
   character(len=128), allocatable, target :: dambreak_ids(:) !< the dambreak ids
   ! Upstream water level
   integer :: nDambreakLocationsUpstream !< nr of dambreak signals with locations upstream
   integer, allocatable :: dambreakLocationsUpstreamMapping(:) !< mapping of dambreak locations upstream
   integer, allocatable :: dambreakLocationsUpstream(:) !< store cell ids for water level locations upstream
   integer :: nDambreakAveragingUpstream !< nr of dambreak signals upstream with averaging
   integer, allocatable :: dambreakAverigingUpstreamMapping(:) !< mapping of dambreak averaging upstream
   ! Downstream water level
   integer :: nDambreakLocationsDownstream !< nr of dambreak signals with locations downstream
   integer, allocatable :: dambreakLocationsDownstreamMapping(:) !< mapping of dambreak locations downstream
   integer, allocatable :: dambreakLocationsDownstream(:) !< store cell ids for water level locations downstream
   integer :: nDambreakAveragingDownstream !< nr of dambreak signals downstream with averaging
   integer, allocatable :: dambreakAverigingDownstreamMapping(:) !< mapping of dambreak averaging in the dambreak arrays

   type polygon
      real(kind=dp), dimension(:), allocatable :: xp, yp
      integer :: np
   end type polygon
   type(polygon), dimension(:), allocatable :: dambreakPolygons

   integer :: nklep !< nr of kleps
   integer, allocatable :: Lklep(:) !< klep links index array, pos=allow 1->2, neg= allow 2->1

   integer :: nvalv !< nr of valvs
   integer, allocatable :: Lvalv(:) !< valv links index array, pos=allow 1->2, neg= allow 2->1
   real(kind=dp), allocatable :: valv(:) !< open fraction of Au

   integer :: nbndqh !< q-h boundary points dimension
   real(kind=dp), allocatable :: xbndqh(:) !< q-h boundary points xcor
   real(kind=dp), allocatable :: ybndqh(:) !< q-h boundary points ycor
   real(kind=dp), allocatable :: zbndqh(:) !< q-h boundary points function
   integer, allocatable :: kdqh(:) !< q-h boundary points temp array
   integer, allocatable :: kbndqh(:, :) !< q-h boundary points index array
                                                        !! 1,* = index in s1 boundary point
                                                        !! 2,* = index in s1 first point on the inside
                                                        !! 3,* = index in u1 of their connecting link (always positive to the inside)
                                                        !! 4,* = type indicator :
                                                        !!                        1 = waterlevel boundary
                                                        !!                        2 = waterlevel neumann
                                                        !!                        3 = velocity   normal ingoing component
                                                        !!                        4 = velocity   flux boundary
                                                        !!                        5 = velocity   Riemann boundary
                                                        !!                        6 = waterlevel outflow
                                                        !!                        7 = q-h boundary
   integer :: nqhbnd !< number of qh boundaries
   character(len=255), allocatable :: qhpliname(:) !< name of the location extracted from the pli-file
   integer, allocatable :: L1qhbnd(:) !< first  nbndz point in discharge bnd nqbnd
   integer, allocatable :: L2qhbnd(:) !< second nbndz point in discharge bnd nqbnd
   real(kind=dp), allocatable, target :: qhbndz(:) !< temporary array for storing boundary values per qh boundary segment
   real(kind=dp), allocatable :: qhbndz_plus(:) !< temporary array for calculating the slope of the QH relation
   real(kind=dp), allocatable :: qhbndz_min(:) !< temporary array for calculating the slope of the QH relation
   real(kind=dp), allocatable, target :: atqh_all(:) !< temporary array for computing discharge through the QH boundary per domain
   real(kind=dp), allocatable :: q_org(:) !< temporary array for saving the discharge through the QH boundary per domain
   real(kind=dp), allocatable :: qh_gamma(:) !< temporary array for saving the slope of the QH-relation
   real(kind=dp), allocatable :: atqh_sum(:) !< temporary array for computing total discharge through qh boundary

   integer :: nwbnd !< number of wave-energy boundaries
   character(len=255), dimension(:), allocatable :: fnamwbnd !< polyline filenames associated with wave-energy boundary

   integer :: numsrc !< nr of point sources/sinks
   integer :: numvalssrc !< nr of point constituents
   integer :: numsrc_nf !< nr of sources/sinks added for nearfield
   integer :: msrc = 0 !< maximal number of points that polylines contains for all sources/sinks
   integer, allocatable :: ksrc(:, :) !< index array, 1=nodenr sink, 2 =kbsin , 3=ktsin, 4 = nodenr source, 5 =kbsor , 6=ktsor
   real(kind=dp), target, allocatable :: qsrc(:) !< cell influx (m3/s) if negative: outflux
   real(kind=dp), allocatable :: sasrc(:) !< q*salinity    (ppt) (m3/s)  if ksrc 3,4 == 0, else delta salinity
   real(kind=dp), allocatable :: tmsrc(:) !< q*temperature (degC) (m3/s) if ksrc 3,4 == 0, else delta temperature
   real(kind=dp), allocatable :: ccsrc(:, :) !< dimension (numvalssrc,numsrc), keeps sasrc, tmsrc etc
   real(kind=dp), allocatable :: qcsrc(:, :) !< q*constituent (c) (m3/s)  )
   real(kind=dp), allocatable :: vcsrc(:, :) !< v*constituent (c) (m3)    )
   real(kind=dp), allocatable :: arsrc(:) !< pipe cross sectional area (m2). If 0, no net momentum
   real(kind=dp), allocatable :: cssrc(:, :) !< (1:2,numsrc) cosine discharge dir pipe on start side (1) and end side (2) of pipe.
   real(kind=dp), allocatable :: snsrc(:, :) !< (1:2,numsrc) sine discharge dir pipe on start side (1) and end side (2) of pipe.
   real(kind=dp), allocatable :: zsrc(:, :) !< vertical level (m) bot
   real(kind=dp), allocatable :: zsrc2(:, :) !< vertical level (m) top (optional)
   real(kind=dp), allocatable :: srsn(:, :) !< 2*(1+numvalssrc),numsrc, to be reduced
   integer, allocatable :: jamess(:) !< issue message mess for from or to point, 0, 1, 2
   real(kind=dp), allocatable, target :: qstss(:) !< array to catch multiple_uni_discharge_salinity_temperature
   character(len=255), allocatable :: srcname(:) !< sources/sinks name (numsrc)
   real(kind=dp), target, allocatable :: vsrccum(:) !< cumulative volume at each source/sink from Tstart to now
   real(kind=dp), allocatable :: vsrccum_pre(:) !< cumulative volume at each source/sink from Tstart to the previous His-output time
   real(kind=dp), target, allocatable :: qsrcavg(:) !< average discharge in the past his-interval at each source/sink
   real(kind=dp), allocatable :: xsrc(:, :) !< x-coordinates of source/sink
   real(kind=dp), allocatable :: ysrc(:, :) !< y-coordinates of source/sink
   integer, allocatable :: nxsrc(:) !< mx nr of points in xsrc, ysrc
   integer, allocatable :: ksrcwaq(:) !< index array, starting point in qsrcwaq
   real(kind=dp), allocatable :: qsrcwaq(:) !< Cumulative qsrc within current waq-timestep
   real(kind=dp), allocatable :: qsrcwaq0(:) !< Cumulative qsrc at the beginning of the time step before possible reduction
   real(kind=dp), allocatable :: qlatwaq(:) !< Cumulative qsrc within current waq-timestep
   real(kind=dp), allocatable :: qlatwaq0(:) !< Cumulative qsrc at the beginning of the time step before possible reduction
   real(kind=dp) :: addksources = 0d0 !< Add k of sources to turkin 1/0

   real(kind=dp), allocatable, target :: sah(:) ! temp
   real(kind=dp), allocatable :: grainlayerthickness(:, :) ! help array grain layer thickness
   integer :: num_lat_ini_blocks
   logical :: tair_available, dewpoint_available
   real(kind=dp), allocatable, target :: uxini(:), uyini(:) !< optional initial velocity fields on u points in x/y dir.
   integer :: inivelx, inively !< set to 1 when initial velocity x or y component is available in *.ext file

contains
!> Sets ALL (scalar) variables in this module to their default values.
!! For external forcings it is equivalent with default_fm_external_forcing_data().
   subroutine reset_flowexternalforcings()
      call default_fm_external_forcing_data()
   end subroutine reset_flowexternalforcings

!> Resets external forcing variables intended for a restart of flow simulation.
!! For external forcings it is equivalent with reset_flowexternalforcings().
   subroutine default_fm_external_forcing_data()
      jatimespace = 0 ! doen ja/nee 1/0
      mhis = 0 ! unit nr external forcings history *.exthis
      numbnp = 0 ! total nr of open boundary cells for network extension
      nopenbndsect = 0 ! Nr. of open boundary sections.
      nbndz = 0 ! waterlevel boundary points dimension
      ndxbnd_own = 0 ! boundary points(without ghost boundary points) dimension
      nbndu = 0 ! velocity   boundary points dimension
      nbndqh = 0 ! q-h boundary points dimension
      nbnds = 0 ! salinity   boundary points dimension
      nbndtm = 0 ! temperature boundary points dimension
      nbndsd = 0 ! sediment   boundary points dimension
      nbndw = 0 ! JRE: wave boundary points dimension
      nbndt = 0 ! tang.velocity boundary points dimension
      nbnduxy = 0 ! uxuy adv vel bnd
      nbndn = 0 ! norm.velocity boundary points dimension

      ngate = 0 ! gates links dimension, to specify gate lower edge level
      ngatesg = 0 ! nr of gate control signals
      ncdam = 0 ! controllable dams nodes dimension, to specify local bottom level
      ncdamsg = 0 ! nr of controllable dam signals
      ncgen = 0 ! general structure nodes dimension, to apply gen struc defs
      ncgensg = 0 ! nr of general structure signals
      ncgen = 0 ! general structure nodes dimension, to apply gen struc defs
      ncgensg = 0 ! nr of general structure signals
      nweirgen = 0 ! nr of weirs in the generalstructure set
      ngategen = 0 ! nr of gates in the generalstructure set
      ngenstru = 0 ! nr of real general structures in the generalstructure set
      npump = 0 ! npump dimension
      npumpsg = 0 ! nr of pump signals
      ndambreaklinks = 0 ! nr of dambreak links
      ndambreaksignals = 0 ! nr of dambreak signals
      nklep = 0 ! nr of kleps
      nvalv = 0 ! nr of valves
      nqbnd = 0 ! nr of q bnd's
      ! JRE
      nzbnd = 0
      nubnd = 0
      numsrc = 0
      numsrc_nf = 0

   end subroutine default_fm_external_forcing_data

end module fm_external_forcings_data
