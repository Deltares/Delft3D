!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module partmem

!     Deltares Software Centre

!     Function            : This is the new memory allocation area.
!                           It is like a huge unnamed common block.
!                           This invites to include it in every routine and make unstructured
!                           use of everything everywhere
!                           That is however not meant.

!     Created             : July    2011 by Leo Postma

      use m_waq_precision       ! single/double precision
      use m_hydmod
      use typos           ! the derived types

      integer(int_wp )  , parameter          :: nfilesp =  100
      integer(int_wp )                       :: lunitp(nfilesp) = 0    ! logical unit numbers for in-/output files
      character(len=256)                :: fnamep(nfilesp) = ' '  ! file names for in-/output files
      logical                           :: alone                  ! if .false. coupled with Delwaq
      integer(int_wp )   :: itrakc  , itraki  , npwndn  , npwndw  , nstep  , nstept
      real   (sp)   :: defang  , hmin    , ptlay   , accrjv
      logical       :: oil     , oil2dh  , oil3d   , ltrack  , acomp  , fout

      type(t_hydrodynamics)              :: hyd           ! description of the hydrodynamics
      integer  ( int_wp )           :: bufsize       ! size of rbuffr
      integer  ( int_wp )           :: nosub_max     ! maximum number of substances
      integer  ( int_wp )           :: nmaxp         ! horizontal dimension 1 of flow file
      integer  ( int_wp )           :: mmaxp         ! horizontal dimension 2 of flow file
      integer  ( int_wp )           :: mnmax2        ! nmax*mmax
      integer  ( int_wp )           :: layt          ! number of layers hydrodynamic model
      logical                  :: fmmodel       ! grid type
      logical                  :: zmodel        ! layer type
      integer  ( int_wp )           :: mnmaxk        ! mnmax2*layt
      integer  ( int_wp )           :: nflow         ! 2*mnmaxk + (layt-1)*mnmax2
      integer  ( int_wp )           :: noseglp       ! either mnmax2 or number of active volumes
      integer  ( int_wp )           :: nosegp        ! either mnmaxk or nosegl*layt
      integer  ( int_wp )           :: noqp          ! either nflow or number of active exchanges
      integer  ( int_wp )           :: ihdel         ! time step in hydrodynamic file
      character( 40)           :: title(4)      ! Simulation title
      integer  ( int_wp )           :: modtyp        ! type of model
      integer  ( int_wp )           :: notrak        ! number of followed particle tracks
      logical  ( int_wp )           :: lsettl        ! if true substances are settling
      integer  ( int_wp )           :: nolayp        ! number of layers   <== pas op
      integer  ( int_wp )           :: noslay        ! number of layers inclusive of optional bed layer
      integer  ( int_wp )           :: idelt         ! simulation time step inputfile
      integer  ( int_wp )           :: ioptdv        ! vertical diffusion option
      real     ( real_wp)           :: alpha         ! scale factor for vertical diffusivity
      real     ( real_wp)           :: cdisp         ! vertical diffusivity constant
      real     ( real_wp)           :: dminim        ! minimum value for vertical diffusion
      logical  ( int_wp )           :: ldiffz        ! switch for vert.diffusion
      integer  ( int_wp )           :: nosubs        ! number of substances in input file
      integer  ( int_wp )           :: nosubc        ! number of substances in conc array
      integer  ( int_wp )           :: nfract        ! number of oil fractions
      real     ( real_wp)           :: pblay         ! relative thickness lower layer
      integer  ( int_wp )           :: itrack        ! substance for particle track
      integer  ( int_wp )           :: ntrack        ! substance for number of particles per bin ?
      integer  ( int_wp )           :: nstick        ! number of sticking substances
      integer  ( int_wp )           :: nopart        ! number of particles
      integer  ( int_wp )           :: nopart_res    ! number of restart particles
      integer  ( int_wp )           :: npmax         ! maximum number of particles
      integer  ( int_wp )           :: npolmax       ! maximum number of polygons in initial conditions
      real     ( real_wp)           :: rough         ! roughness length
      real     ( real_wp)           :: drand  (3)    ! random step parameters
      logical  ( int_wp )           :: spawnd        ! if true space varying wind
      integer  ( int_wp )           :: nowind        ! number of wind breakpoints
      integer  ( int_wp )           :: noconsp       ! number of constants in the const array
      integer  ( int_wp )           :: itstrtp       ! simulation start time
      integer  ( int_wp )           :: itstopp       ! simulation stop time
      integer  ( int_wp )           :: iddtim        ! DELWAQ delay time
      integer  ( int_wp )           :: icwsta        ! map file start time
      integer  ( int_wp )           :: icwsto        ! map file stop time
      integer  ( int_wp )           :: icwste        ! map file time step
      integer  ( int_wp )           :: ihstrtp       ! start time-histories
      integer  ( int_wp )           :: ihstopp       ! stop time-histories
      integer  ( int_wp )           :: ihstepp       ! time-step on history file
      integer  ( int_wp )           :: iyear         ! year of the calendar offset
      integer  ( int_wp )           :: imonth        ! month of the calendar offset
      integer  ( int_wp )           :: iofset        ! seconds offset to calendar
      logical  ( int_wp )           :: ldiffh        ! switch for hor. diffusion
      real     ( real_wp)           :: rhow          ! density of water in g/l (= kg/m3)
      integer  ( int_wp )           :: stickdf       ! sticking at drying flats
      integer  ( int_wp )           :: oil_opt       ! option for initial conditions of oil
      integer  ( int_wp )           :: ini_opt       ! option for initial conditions of oil
      character(256)           :: ini_file      ! file name initial conditions of oil
      character(256)           :: idp_file      ! file name initial conditions of delpar
      character(256)           :: size_file     ! file name for dump of particle sizes
      character(256)           :: res_file      ! file name for restart file written a the end of a run
      integer  ( int_wp )           :: tydisp        ! type of dispersant application effectiveness parameter
      integer  ( int_wp )           :: ndisapp       ! number of dispersant applications
      integer  ( int_wp )           :: tyboom        ! type of boom effectiveness parameter
      integer  ( int_wp )           :: nboomint      ! number of boom introductions
      integer  ( int_wp )           :: nosta         ! number of monitoring stations
      integer  ( int_wp )           :: iptset        ! number of plotgrids
      real     ( real_wp)           :: window(4)     ! plotgrid window coordinates
      integer  ( int_wp )           :: mmap          ! plotgrid resolution
      integer  ( int_wp )           :: nmap          ! plotgrid resolution
      integer  ( int_wp )           :: nodye         ! number of dye releases
      integer  ( int_wp )           :: nocont        ! number of continuous releases
      integer  ( int_wp )           :: noudef        ! number of user defined releases
      integer  ( int_wp )           :: idtset        ! number of time points decay rates
      real     ( real_wp)           :: anfac         !
      integer  ( int_wp )           :: irfac         !
      integer  ( int_wp )           :: nrowsmax      !
      integer  ( int_wp )           :: ivtset        ! number of time points settling velocities
      real     ( real_wp)           :: chezy         ! chezy value
      real     ( real_wp)           :: taucs         ! critical tau sedimentation
      real     ( real_wp)           :: tauce         ! critical tau erosion
      logical                  :: caltau        ! if .true. calculate tau

      integer  ( int_wp ), pointer  :: lgrid (:,:)   ! active grid matrix, with 1-1 numbering
      integer  ( int_wp ), pointer  :: lgrid2(:,:)   ! total grid matrix
      integer  ( int_wp ), pointer  :: lgrid3(:,:)   ! active grid matrix with noseg numbering
      integer  ( int_wp ), pointer  :: laytop(:,:)   ! highest active layer in z-layer model
      integer  ( int_wp ), pointer  :: laytopp(:,:)  ! highest active layer in z-layer model on previous time step
      integer  ( int_wp ), pointer  :: laybot(:,:)   ! deepest active layer in z-layer model
      integer  ( int_wp ), pointer  :: pagrid(:,:,:) ! potentially active z-layer segments grid matrix
      integer  ( int_wp ), pointer  :: aagrid(:,:,:) ! actually active z-layer segments grid matrix
      real     ( real_wp), pointer  :: tcktot (:)    ! relative layer thickness
      real     ( real_wp), pointer  :: zlbot (:)     ! z-layer layer bottom level
      real     ( real_wp), pointer  :: zltop (:)     ! z-layer layer top level
      integer  ( int_wp ), pointer  :: cellpntp(:)   ! pointer from noseg to mnmaxk
      integer  ( int_wp ), pointer  :: flowpntp(:,:) ! pointer from noq to nflow
      real     ( real_wp), pointer  :: angle  (:)    !
      real     ( real_wp), pointer  :: area   (:)    !
      real     ( real_wp), pointer  :: depth  (:)    !
      real     ( real_wp), pointer  :: dpsp   (:)    !
      real     ( real_wp), pointer  :: dx     (:)    !
      real     ( real_wp), pointer  :: dy     (:)    !
      real     ( real_wp), pointer  :: flow   (:)    !
      real     ( real_wp), pointer  :: flow2m (:)    !
      real     ( real_wp), pointer  :: flow1  (:)    !
      real     ( real_wp), pointer  :: flow2  (:)    !
      integer  ( int_wp ), pointer  :: ipntp  (:)    !
      integer  ( int_wp ), pointer  :: nplay  (:)    !
      real     ( real_wp), pointer  :: vdiff  (:)    ! vertical diffusion
      real     ( real_wp), pointer  :: vdiff1 (:)    ! vertical diffusion from file
      real     ( real_wp), pointer  :: tau    (:)    ! tau
      real     ( real_wp), pointer  :: tau1   (:)    ! tau from file
      real     ( real_wp), pointer  :: salin  (:)    ! salinity
      real     ( real_wp), pointer  :: salin1 (:)    ! salinity from file
      real     ( real_wp), pointer  :: rhowatc (:) ! density water

      real     ( real_wp), pointer  :: temper (:)    ! temperature
      real     ( real_wp), pointer  :: temper1(:)    ! temperature from file
      real     ( real_wp), pointer  :: vel1   (:)    ! velocity at begin (first layer only)
      real     ( real_wp), pointer  :: vel2   (:)    ! velocity at end (first layer only)
      real     ( real_wp), pointer  :: velo   (:)    !
      real     ( real_wp), pointer  :: vol1   (:)    !
      real     ( real_wp), pointer  :: vol2   (:)    !
      real     ( real_wp), pointer  :: volumep(:)    !
      real     ( real_wp), pointer  :: xb     (:)    !
      real     ( real_wp), pointer  :: yb     (:)    !
      real     ( real_wp), pointer  :: zlevel (:)    !
      real     ( real_wp), pointer  :: locdepp(:,:)   !
      real     ( real_wp), pointer  :: locdep(:,:)   !
      character( 20), pointer  :: substi (:)    ! substances' names input file
      integer  ( int_wp ), pointer  :: mapsub (:)    ! gives substances a number for output
      integer  ( int_wp ), pointer  :: nplot  (:)    ! seq. ordered particle numbers for tracks
      integer  ( int_wp ), pointer  :: mstick (:)    ! array that tells if a substance i is sticking
      character( 20), pointer  :: subst  (:)    ! substances' names output file
      character( 20), pointer  :: subst2 (:)    ! substances' names output file
      real     ( real_wp), pointer  :: wveloa (:)    ! wind velocity  m/s
      real     ( real_wp), pointer  :: wdira  (:)    ! wind direction degree from north
      real     ( dp), pointer  :: wvelo  (:)    ! space varying wind velocity  m/s
      real     ( dp), pointer  :: wdir   (:)    ! space varying wind direction degree from north
      integer  ( int_wp ), pointer  :: iwndtm (:)    ! breakpoints wind time series
      real     ( real_wp), pointer  :: const  (:)    ! constant factors
      character( 20), pointer  :: nmstat (:)    ! names of the monitoring stations
      real     ( real_wp), pointer  :: xstat  (:)    ! x-values monitoring stations
      real     ( real_wp), pointer  :: ystat  (:)    ! y-values monitoring stations
      integer  ( int_wp ), pointer  :: ipset  (:)    ! plot grid timings
      real     ( real_wp), pointer  :: recovr (:)    ! recovery rates to be applied for the plot grids
      integer  ( int_wp ), pointer  :: idisset(:)    ! timing of dispersant application
      real     ( sp), pointer  :: efdisp (:,:)  ! effectiveness parameter of dispersant application per oil type
      character( 256),pointer  :: fidisp (:)    ! names of dispersant polygon files
      real     ( sp), pointer  :: xpoldis (:,:) ! x-coordinates of dispersant polygon
      real     ( sp), pointer  :: ypoldis (:,:) ! y-coordinates of dispersant polygon
      integer  ( int_wp ), pointer  :: nrowsdis (:)  ! length of dispersant polygon
      integer  ( int_wp ), pointer  :: iboomset(:)   ! timing of boom introduction
      real     ( sp), pointer  :: efboom (:,:)  ! effectiveness parameter of boom per oil type
      real     ( sp), pointer  :: xpolboom (:,:)! x-coordinates of boom polygon
      real     ( sp), pointer  :: ypolboom (:,:)! y-coordinates of boom polygon
      integer  ( int_wp ), pointer  :: nrowsboom (:) ! length of dispersant polygon
      character( 256),pointer  :: fiboom (:)    ! names of boom polygon files
      character( 20), pointer  :: nmdyer (:)    ! names of the dye releases
      integer  ( int_wp ), pointer  :: iwtime (:)    ! times per dye release
      real     ( real_wp), pointer  :: xwaste (:)    ! x of waste point
      real     ( real_wp), pointer  :: ywaste (:)    ! y of waste point
      real     ( real_wp), pointer  :: zwaste (:)    ! z of waste point
      integer  ( int_wp ), pointer  :: kwaste (:)    ! layer nr of waste point
      integer  ( int_wp ), pointer  :: ioptrad(:)    ! radius option of dye release
      real     ( real_wp), pointer  :: radius (:)    ! radius parameter of waste point
      character( 256),pointer  :: fidye(:)      ! temporary array with names of dye polygon files
      character( 256),pointer  :: fiwaste(:)    ! names of waste polygon files
      real     ( sp), pointer  :: xpolwaste(:,:)! x-coordinates of waste polygon
      real     ( sp), pointer  :: ypolwaste(:,:)! y-coordinates of waste polygon
      integer  ( int_wp ), pointer  :: nrowswaste(:) ! length of waste polygon
      real     ( real_wp), pointer  :: wparm  (:)    ! percentage of particles taken
      integer  ( int_wp ), pointer  :: ndprt  (:)    ! number of particles per waste point
      real     ( real_wp), pointer  :: amassd(:,:)   ! mass of dye per substance
      character( 20), pointer  :: nmconr (:)    ! names of the continuous releases
      integer  ( int_wp ), pointer  :: linear (:)    ! interpolation method of continuous releases
      real     ( real_wp), pointer  :: stoch (:,:)   ! stochi of continuous loads per substance
      integer  ( int_wp ), pointer  :: ictmax (:)    ! number of time points per continuous load
      integer  ( int_wp ), pointer  :: ictime(:,:)   ! time series per continuous load
      real     ( real_wp), pointer  :: amassc(:,:,:) ! mass of continuous load per substance per time step
      real     ( real_wp), pointer  :: ftime (:,:)   ! time matrix continuous loads in 1/s
      real     ( real_wp), pointer  :: uscal  (:)    ! scale values user defined releases
      integer  ( int_wp ), pointer  :: isubud (:)    ! index array for substances for user defined releases
      integer  ( int_wp ), pointer  :: iutime (:)    ! user defined releases release times
      integer  ( int_wp ), pointer  :: ifopt  (:)    ! file option user defined releases
      character(256), pointer  :: finud  (:)    ! filenames of user defined delwaq files
      integer  ( int_wp ), pointer  :: iftime (:)    ! user defined releases reading times from files
      integer  ( int_wp ), pointer  :: nosud  (:)    ! number of subst. on file for ud release (ifopt=1)
      integer  ( int_wp ), pointer  :: isfud  (:)    ! index array for subst. from files ud rel.
      integer  ( int_wp ), pointer  :: idtime (:)    ! array with time points for decay
      real     ( real_wp), pointer  :: decay (:,:)   ! matrix of decays per substance per time point
      real     ( real_wp), pointer  :: decays (:)    ! the actual decay values per substance at this time
      integer  ( int_wp ), pointer  :: ivtime (:)    ! array with time points for settling velocities
      real     ( sp), pointer  :: wpart (:,:)   ! weight of the substances in each particle
      real     ( real_wp), pointer  :: wpartini (:,:)   ! weight of the substances in each particle
      real     ( sp), pointer  :: spart (:,:)   ! size of the particles
      real     ( real_wp), pointer  :: rhopart (:,:) ! density of the substances in each particle

      real     ( real_wp), pointer  :: vsfour(:,:,:) ! matrix with fourier coefficients settling
      real     ( real_wp), pointer  :: wsettl (:)    ! settling velocity per particel at this time
      integer  ( int_wp )              nbmax         ! maximum amount of real open boundaries
      integer  ( int_wp )              ndoms         ! number of domains
      type  (domain), pointer  :: doms   (:)    ! the domains
      integer  ( int_wp )              nbnds         ! number of inter domain boundaries
      type  (boundp), pointer  :: bnds   (:)    ! the inter domain boundaries
      integer  ( int_wp )              nconn         ! number of links
      type  (pnt   ), pointer  :: conn   (:)    ! the DD links
      integer  ( int_wp )              npgrid        ! number of plotgrids
      type  (PlotGrid),pointer :: pg     (:)

      real     ( real_wp), pointer  :: t0cf   (:)    !
      real     ( real_wp), pointer  :: tmassu (:)    !
      real     ( real_wp), pointer  :: acf    (:)    !
      integer  ( int_wp ), pointer  :: ncheck (:)    !
      real     ( real_wp), pointer  :: rem    (:)    !
      real     ( real_wp), pointer  :: tmassc(:,:)   !
      real     ( real_wp), pointer  :: aconc (:,:)   !
      character     (len=20   ) ,  pointer, dimension(:       ) :: cbuff
      character     (len=20   ) ,  pointer, dimension(:       ) :: subsud
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: floil
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: ihplot
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: iptime
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: isfile
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: mpart
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: mpart0
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: mplsta
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: mstat
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: mwaste
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: npart0
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: nplsta
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: nstat
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: nwaste
      integer       (int_wp        ) ,  pointer, dimension(:,:     ) :: imap
      integer       (int_wp        ) ,  pointer, dimension(:,:     ) :: imask
      integer       (int_wp        ) ,  pointer, dimension(:,:     ) :: ibuff
      integer       (int_wp        ) ,  pointer, dimension(:,:     ) :: mcell
      integer       (int_wp        ) ,  pointer, dimension(:,:     ) :: ncell
      integer       (int_wp        ) ,  pointer, dimension(:,:,:   ) :: nbin
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: nosyss
      real          (sp       ) ,  pointer, dimension(:       ) :: abuoy
      real          (sp       ) ,  pointer, dimension(:       ) :: dfact
      real          (sp       ) ,  pointer, dimension(:       ) :: fstick
      real          (sp       ) ,  pointer, dimension(:       ) :: t0buoy
      real          (sp       ) ,  pointer, dimension(:       ) :: tmass
      real          (sp       ) ,  pointer, dimension(:       ) :: xa
      real          (sp       ) ,  pointer, dimension(:       ) :: xa0
      real          (sp       ) ,  pointer, dimension(:       ) :: xpart0
      real          (sp       ) ,  pointer, dimension(:       ) :: ya
      real          (sp       ) ,  pointer, dimension(:,:     ) :: tmasud
      real          (sp       ) ,  pointer, dimension(:       ) :: ya0
      real          (sp       ) ,  pointer, dimension(:       ) :: ypart0
      real          (sp       ) ,  pointer, dimension(:       ) :: za
      real          (sp       ) ,  pointer, dimension(:,:     ) :: aconud
      real          (sp       ) ,  pointer, dimension(:,:     ) :: adepth
      real          (sp       ) ,  pointer, dimension(:,:     ) :: apeak
      real          (sp       ) ,  pointer, dimension(:,:     ) :: atotal
      real          (sp       ) ,  pointer, dimension(:,:     ) :: rbuff
      real          (sp       ) ,  pointer, dimension(:,:     ) :: track
      real          (sp       ) ,  pointer, dimension(:,:     ) :: vrtdsp
      real          (sp       ) ,  pointer, dimension(:,:     ) :: vsfact
      real          (sp       ) ,  pointer, dimension(:,:     ) :: xyztrk
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: atrack
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: chismp
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: chispl
      real          (sp       ) ,  pointer, dimension(:,:,:,: ) :: amap
      real          (sp       ) ,  pointer, dimension(:,:,:,: ) :: amapsett
      real          (sp       ) ,  pointer, dimension(:       ) :: xpol
      real          (sp       ) ,  pointer, dimension(:       ) :: ypol
      real          (sp       ) ,  pointer, dimension(:       ) :: conc2
      character     (len=16   ) ,  pointer, dimension(:       ) :: elt_names
      character     (len=16   ) ,  pointer, dimension(:       ) :: elt_types
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: elt_bytes
      integer       (int_wp        ) ,  pointer, dimension(:,:     ) :: elt_dims
      real          (sp       ) ,  pointer, dimension(:       ) :: rbuffr
      real          (sp       ) ,  pointer, dimension(:,:     ) :: concp
      real          (sp       ) ,  pointer, dimension(:,:     ) :: flres

      real     ( real_wp), pointer  :: v_swim(:)    ! horizontal swim velocity (m/s)
      real     ( real_wp), pointer  :: d_swim(:)    ! horizontal swim direction (degree)
end module partmem

module m_part_regular
      use m_waq_precision       ! single/double precision
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: npart
      real          (sp       ) ,  pointer, dimension(:       ) :: xpart
      real          (sp       ) ,  pointer, dimension(:       ) :: ypart
      real          (sp       ) ,  pointer, dimension(:       ) :: zpart
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: kpart
      integer       (int_wp        ) ,  pointer, dimension(:       ) :: isub

end module m_part_regular

module spec_feat_par

      !     special feature parameters

      use m_waq_precision      ! single and double precision

!     vertical bounce
      logical                                                   :: vertical_bounce

!     wind drag for all particles
      logical                                                   :: apply_wind_drag
      real      (sp)                                            :: max_wind_drag_depth

!     restart files
      logical                                                   :: write_restart_file
      integer  (int_wp )                                             :: max_restart_age

!     plastics parameters
      integer   (int_wp )            ,  pointer, dimension(:       ) :: plparset
      real      (sp)            ,  pointer, dimension(:       ) :: pldensity
      real      (sp)            ,  pointer, dimension(:       ) :: plshapefactor
      real      (sp)            ,  pointer, dimension(:       ) :: plmeansize
      real      (sp)            ,  pointer, dimension(:       ) :: plvarsize
      real      (sp)            ,  pointer, dimension(:       ) :: plmusize
      real      (sp)            ,  pointer, dimension(:       ) :: plsigmasize
      real      (sp)            ,  pointer, dimension(:       ) :: plfragrate
      logical                                                   :: pldebug

!     screens
      logical                  :: screens          ! are sceens active
      real     ( sp)           :: permealeft       ! leftside permeability of screeens
      real     ( sp)           :: permearight      ! rightside permeability of screeens
      character( 256)          :: fiscreens        ! names of screens polygon files
      integer  ( int_wp )           :: nrowsscreens     ! length of screen polygon
      real     ( sp), pointer  :: xpolscreens(:)   ! x-coordinates of screen polygon
      real     ( sp), pointer  :: ypolscreens(:)   ! y-coordinates of screen polygon

!     ABM
      logical                  :: abmmodel          ! is ABM keyword active
      logical                  :: chronrev          ! is chronology reversed
      character( 256)          :: abmmodelname      ! name of ABM model used
      character( 256)          :: abmstagedev       ! name of ABM model stage developement used
      integer  ( sp)           :: abmmt             ! nr of ABM model used
      integer  ( sp)           :: abmsd             ! nr of ABM model stage developement used
      real     ( sp)           :: selstage          ! nr of ABM model stage for chronology reversed model
end module spec_feat_par
