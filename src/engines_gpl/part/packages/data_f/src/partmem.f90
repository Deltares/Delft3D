!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      use precision_part       ! single/double precision
      use hydmod
      use typos           ! the derived types

      integer(ip)  , parameter          :: nfilesp =  100
      integer(ip)                       :: lunitp(nfilesp) = 0    ! logical unit numbers for in-/output files
      character(len=256)                :: fnamep(nfilesp) = ' '  ! file names for in-/output files
      logical                           :: alone                  ! if .false. coupled with Delwaq
      integer(ip)   :: itrakc  , itraki  , npwndn  , npwndw  , nstep  , nstept
      real   (sp)   :: defang  , hmin    , ptlay   , accrjv
      logical       :: oil     , oil2dh  , oil3d   , ltrack  , acomp  , fout

      type(t_hyd)              :: hyd           ! description of the hydrodynamics
      integer  ( ip)           :: bufsize       ! size of rbuffr
      integer  ( ip)           :: nosub_max     ! maximum number of substances
      integer  ( ip)           :: nmaxp         ! horizontal dimension 1 of flow file
      integer  ( ip)           :: mmaxp         ! horizontal dimension 2 of flow file
      integer  ( ip)           :: mnmax2        ! nmax*mmax
      integer  ( ip)           :: layt          ! number of layers hydrodynamic model
      logical                  :: fmmodel       ! grid type
      logical                  :: zmodel        ! layer type
      integer  ( ip)           :: mnmaxk        ! mnmax2*layt
      integer  ( ip)           :: nflow         ! 2*mnmaxk + (layt-1)*mnmax2
      integer  ( ip)           :: noseglp       ! either mnmax2 or number of active volumes
      integer  ( ip)           :: nosegp        ! either mnmaxk or nosegl*layt
      integer  ( ip)           :: noqp          ! either nflow or number of active exchanges
      integer  ( ip)           :: ihdel         ! time step in hydrodynamic file
      character( 40)           :: title(4)      ! Simulation title
      integer  ( ip)           :: modtyp        ! type of model
      integer  ( ip)           :: notrak        ! number of followed particle tracks
      logical  ( ip)           :: lsettl        ! if true substances are settling
      integer  ( ip)           :: nolayp        ! number of layers   <== pas op
      integer  ( ip)           :: noslay        ! number of layers inclusive of optional bed layer
      integer  ( ip)           :: idelt         ! simulation time step inputfile
      integer  ( ip)           :: ioptdv        ! vertical diffusion option
      real     ( rp)           :: alpha         ! scale factor for vertical diffusivity
      real     ( rp)           :: cdisp         ! vertical diffusivity constant
      real     ( rp)           :: dminim        ! minimum value for vertical diffusion
      logical  ( ip)           :: ldiffz        ! switch for vert.diffusion
      integer  ( ip)           :: nosubs        ! number of substances in input file
      integer  ( ip)           :: nosubc        ! number of substances in conc array
      integer  ( ip)           :: nfract        ! number of oil fractions
      real     ( rp)           :: pblay         ! relative thickness lower layer
      integer  ( ip)           :: itrack        ! substance for particle track
      integer  ( ip)           :: ntrack        ! substance for number of particles per bin ?
      integer  ( ip)           :: nstick        ! number of sticking substances
      integer  ( ip)           :: nopart        ! number of particles
      integer  ( ip)           :: nopart_res    ! number of restart particles
      integer  ( ip)           :: npmax         ! maximum number of particles
      integer  ( ip)           :: npolmax       ! maximum number of polygons in initial conditions
      real     ( rp)           :: rough         ! roughness length
      real     ( rp)           :: drand  (3)    ! random step parameters
      logical  ( ip)           :: spawnd        ! if true space varying wind
      integer  ( ip)           :: nowind        ! number of wind breakpoints
      integer  ( ip)           :: noconsp       ! number of constants in the const array
      integer  ( ip)           :: itstrtp       ! simulation start time
      integer  ( ip)           :: itstopp       ! simulation stop time
      integer  ( ip)           :: iddtim        ! DELWAQ delay time
      integer  ( ip)           :: icwsta        ! map file start time
      integer  ( ip)           :: icwsto        ! map file stop time
      integer  ( ip)           :: icwste        ! map file time step
      integer  ( ip)           :: ihstrtp       ! start time-histories
      integer  ( ip)           :: ihstopp       ! stop time-histories
      integer  ( ip)           :: ihstepp       ! time-step on history file
      integer  ( ip)           :: iyear         ! year of the calendar offset
      integer  ( ip)           :: imonth        ! month of the calendar offset
      integer  ( ip)           :: iofset        ! seconds offset to calendar
      logical  ( ip)           :: ldiffh        ! switch for hor. diffusion
      real     ( rp)           :: rhow          ! density of water in g/l (= kg/m3)
      integer  ( ip)           :: stickdf       ! sticking at drying flats
      integer  ( ip)           :: oil_opt       ! option for initial conditions of oil
      integer  ( ip)           :: ini_opt       ! option for initial conditions of oil
      character(256)           :: ini_file      ! file name initial conditions of oil
      character(256)           :: idp_file      ! file name initial conditions of delpar
      character(256)           :: size_file     ! file name for dump of particle sizes
      character(256)           :: res_file      ! file name for restart file written a the end of a run
      integer  ( ip)           :: tydisp        ! type of dispersant application effectiveness parameter
      integer  ( ip)           :: ndisapp       ! number of dispersant applications
      integer  ( ip)           :: tyboom        ! type of boom effectiveness parameter
      integer  ( ip)           :: nboomint      ! number of boom introductions
      integer  ( ip)           :: nosta         ! number of monitoring stations
      integer  ( ip)           :: iptset        ! number of plotgrids
      real     ( rp)           :: window(4)     ! plotgrid window coordinates
      integer  ( ip)           :: mmap          ! plotgrid resolution
      integer  ( ip)           :: nmap          ! plotgrid resolution
      integer  ( ip)           :: nodye         ! number of dye releases
      integer  ( ip)           :: nocont        ! number of continuous releases
      integer  ( ip)           :: noudef        ! number of user defined releases
      integer  ( ip)           :: idtset        ! number of time points decay rates
      real     ( rp)           :: anfac         !
      integer  ( ip)           :: irfac         !
      integer  ( ip)           :: nrowsmax      !
      integer  ( ip)           :: ivtset        ! number of time points settling velocities
      real     ( rp)           :: chezy         ! chezy value
      real     ( rp)           :: taucs         ! critical tau sedimentation
      real     ( rp)           :: tauce         ! critical tau erosion
      logical                  :: caltau        ! if .true. calculate tau

      integer  ( ip), pointer  :: lgrid (:,:)   ! active grid matrix, with 1-1 numbering
      integer  ( ip), pointer  :: lgrid2(:,:)   ! total grid matrix
      integer  ( ip), pointer  :: lgrid3(:,:)   ! active grid matrix with noseg numbering
      integer  ( ip), pointer  :: laytop(:,:)   ! highest active layer in z-layer model
      integer  ( ip), pointer  :: laytopp(:,:)  ! highest active layer in z-layer model on previous time step
      integer  ( ip), pointer  :: laybot(:,:)   ! deepest active layer in z-layer model
      integer  ( ip), pointer  :: pagrid(:,:,:) ! potentially active z-layer segments grid matrix
      integer  ( ip), pointer  :: aagrid(:,:,:) ! actually active z-layer segments grid matrix
      real     ( rp), pointer  :: tcktot (:)    ! relative layer thickness
      real     ( rp), pointer  :: zlbot (:)     ! z-layer layer bottom level
      real     ( rp), pointer  :: zltop (:)     ! z-layer layer top level
      integer  ( ip), pointer  :: cellpntp(:)   ! pointer from noseg to mnmaxk
      integer  ( ip), pointer  :: flowpntp(:,:) ! pointer from noq to nflow
      real     ( rp), pointer  :: angle  (:)    !
      real     ( rp), pointer  :: area   (:)    !
      real     ( rp), pointer  :: depth  (:)    !
      real     ( rp), pointer  :: dpsp   (:)    !
      real     ( rp), pointer  :: dx     (:)    !
      real     ( rp), pointer  :: dy     (:)    !
      real     ( rp), pointer  :: flow   (:)    !
      real     ( rp), pointer  :: flow2m (:)    !
      real     ( rp), pointer  :: flow1  (:)    !
      real     ( rp), pointer  :: flow2  (:)    !
      integer  ( ip), pointer  :: ipntp  (:)    !
      integer  ( ip), pointer  :: nplay  (:)    !
      real     ( rp), pointer  :: vdiff  (:)    ! vertical diffusion
      real     ( rp), pointer  :: vdiff1 (:)    ! vertical diffusion from file
      real     ( rp), pointer  :: tau    (:)    ! tau
      real     ( rp), pointer  :: tau1   (:)    ! tau from file
      real     ( rp), pointer  :: salin  (:)    ! salinity
      real     ( rp), pointer  :: salin1 (:)    ! salinity from file
      real     ( rp), pointer  :: rhowatc (:) ! density water

      real     ( rp), pointer  :: temper (:)    ! temperature
      real     ( rp), pointer  :: temper1(:)    ! temperature from file
      real     ( rp), pointer  :: vel1   (:)    ! velocity at begin (first layer only)
      real     ( rp), pointer  :: vel2   (:)    ! velocity at end (first layer only)
      real     ( rp), pointer  :: velo   (:)    !
      real     ( rp), pointer  :: vol1   (:)    !
      real     ( rp), pointer  :: vol2   (:)    !
      real     ( rp), pointer  :: volumep(:)    !
      real     ( rp), pointer  :: xb     (:)    !
      real     ( rp), pointer  :: yb     (:)    !
      real     ( rp), pointer  :: zlevel (:)    !
      real     ( rp), pointer  :: locdepp(:,:)   !
      real     ( rp), pointer  :: locdep(:,:)   !
      character( 20), pointer  :: substi (:)    ! substances' names input file
      integer  ( ip), pointer  :: mapsub (:)    ! gives substances a number for output
      integer  ( ip), pointer  :: nplot  (:)    ! seq. ordered particle numbers for tracks
      integer  ( ip), pointer  :: mstick (:)    ! array that tells if a substance i is sticking
      character( 20), pointer  :: subst  (:)    ! substances' names output file
      character( 20), pointer  :: subst2 (:)    ! substances' names output file
      real     ( rp), pointer  :: wveloa (:)    ! wind velocity  m/s
      real     ( rp), pointer  :: wdira  (:)    ! wind direction degree from north
      real     ( hp), pointer  :: wvelo  (:)    ! space varying wind velocity  m/s
      real     ( hp), pointer  :: wdir   (:)    ! space varying wind direction degree from north
      integer  ( ip), pointer  :: iwndtm (:)    ! breakpoints wind time series
      real     ( rp), pointer  :: const  (:)    ! constant factors
      character( 20), pointer  :: nmstat (:)    ! names of the monitoring stations
      real     ( rp), pointer  :: xstat  (:)    ! x-values monitoring stations
      real     ( rp), pointer  :: ystat  (:)    ! y-values monitoring stations
      integer  ( ip), pointer  :: ipset  (:)    ! plot grid timings
      real     ( rp), pointer  :: recovr (:)    ! recovery rates to be applied for the plot grids
      integer  ( ip), pointer  :: idisset(:)    ! timing of dispersant application
      real     ( sp), pointer  :: efdisp (:,:)  ! effectiveness parameter of dispersant application per oil type
      character( 256),pointer  :: fidisp (:)    ! names of dispersant polygon files
      real     ( sp), pointer  :: xpoldis (:,:) ! x-coordinates of dispersant polygon
      real     ( sp), pointer  :: ypoldis (:,:) ! y-coordinates of dispersant polygon
      integer  ( ip), pointer  :: nrowsdis (:)  ! length of dispersant polygon
      integer  ( ip), pointer  :: iboomset(:)   ! timing of boom introduction
      real     ( sp), pointer  :: efboom (:,:)  ! effectiveness parameter of boom per oil type
      real     ( sp), pointer  :: xpolboom (:,:)! x-coordinates of boom polygon
      real     ( sp), pointer  :: ypolboom (:,:)! y-coordinates of boom polygon
      integer  ( ip), pointer  :: nrowsboom (:) ! length of dispersant polygon
      character( 256),pointer  :: fiboom (:)    ! names of boom polygon files
      character( 20), pointer  :: nmdyer (:)    ! names of the dye releases
      integer  ( ip), pointer  :: iwtime (:)    ! times per dye release
      real     ( rp), pointer  :: xwaste (:)    ! x of waste point
      real     ( rp), pointer  :: ywaste (:)    ! y of waste point
      real     ( rp), pointer  :: zwaste (:)    ! z of waste point
      integer  ( ip), pointer  :: kwaste (:)    ! layer nr of waste point
      integer  ( ip), pointer  :: ioptrad(:)    ! radius option of dye release
      real     ( rp), pointer  :: radius (:)    ! radius parameter of waste point
      character( 256),pointer  :: fidye(:)      ! temporary array with names of dye polygon files
      character( 256),pointer  :: fiwaste(:)    ! names of waste polygon files
      real     ( sp), pointer  :: xpolwaste(:,:)! x-coordinates of waste polygon
      real     ( sp), pointer  :: ypolwaste(:,:)! y-coordinates of waste polygon
      integer  ( ip), pointer  :: nrowswaste(:) ! length of waste polygon
      real     ( rp), pointer  :: wparm  (:)    ! percentage of particles taken
      integer  ( ip), pointer  :: ndprt  (:)    ! number of particles per waste point
      real     ( rp), pointer  :: amassd(:,:)   ! mass of dye per substance
      character( 20), pointer  :: nmconr (:)    ! names of the continuous releases
      integer  ( ip), pointer  :: linear (:)    ! interpolation method of continuous releases
      real     ( rp), pointer  :: stoch (:,:)   ! stochi of continuous loads per substance
      integer  ( ip), pointer  :: ictmax (:)    ! number of time points per continuous load
      integer  ( ip), pointer  :: ictime(:,:)   ! time series per continuous load
      real     ( rp), pointer  :: amassc(:,:,:) ! mass of continuous load per substance per time step
      real     ( rp), pointer  :: ftime (:,:)   ! time matrix continuous loads in 1/s
      real     ( rp), pointer  :: uscal  (:)    ! scale values user defined releases
      integer  ( ip), pointer  :: isubud (:)    ! index array for substances for user defined releases
      integer  ( ip), pointer  :: iutime (:)    ! user defined releases release times
      integer  ( ip), pointer  :: ifopt  (:)    ! file option user defined releases
      character(256), pointer  :: finud  (:)    ! filenames of user defined delwaq files
      integer  ( ip), pointer  :: iftime (:)    ! user defined releases reading times from files
      integer  ( ip), pointer  :: nosud  (:)    ! number of subst. on file for ud release (ifopt=1)
      integer  ( ip), pointer  :: isfud  (:)    ! index array for subst. from files ud rel.
      integer  ( ip), pointer  :: idtime (:)    ! array with time points for decay
      real     ( rp), pointer  :: decay (:,:)   ! matrix of decays per substance per time point
      real     ( rp), pointer  :: decays (:)    ! the actual decay values per substance at this time
      integer  ( ip), pointer  :: ivtime (:)    ! array with time points for settling velocities
      real     ( sp), pointer  :: wpart (:,:)   ! weight of the substances in each particle
      real     ( rp), pointer  :: wpartini (:,:)   ! weight of the substances in each particle
      real     ( sp), pointer  :: spart (:,:)   ! size of the particles
      real     ( rp), pointer  :: rhopart (:,:) ! density of the substances in each particle

      real     ( rp), pointer  :: vsfour(:,:,:) ! matrix with fourier coefficients settling
      real     ( rp), pointer  :: wsettl (:)    ! settling velocity per particel at this time
      integer  ( ip)              nbmax         ! maximum amount of real open boundaries
      integer  ( ip)              ndoms         ! number of domains
      type  (domain), pointer  :: doms   (:)    ! the domains
      integer  ( ip)              nbnds         ! number of inter domain boundaries
      type  (boundp), pointer  :: bnds   (:)    ! the inter domain boundaries
      integer  ( ip)              nconn         ! number of links
      type  (pnt   ), pointer  :: conn   (:)    ! the DD links
      integer  ( ip)              npgrid        ! number of plotgrids
      type  (PlotGrid),pointer :: pg     (:)

      real     ( rp), pointer  :: t0cf   (:)    !
      real     ( rp), pointer  :: tmassu (:)    !
      real     ( rp), pointer  :: acf    (:)    !
      integer  ( ip), pointer  :: ncheck (:)    !
      real     ( rp), pointer  :: rem    (:)    !
      real     ( rp), pointer  :: tmassc(:,:)   !
      real     ( rp), pointer  :: aconc (:,:)   !
      character     (len=20   ) ,  pointer, dimension(:       ) :: cbuff
      character     (len=20   ) ,  pointer, dimension(:       ) :: subsud
      integer       (ip       ) ,  pointer, dimension(:       ) :: floil
      integer       (ip       ) ,  pointer, dimension(:       ) :: ihplot
      integer       (ip       ) ,  pointer, dimension(:       ) :: iptime
      integer       (ip       ) ,  pointer, dimension(:       ) :: isfile
      integer       (ip       ) ,  pointer, dimension(:       ) :: mpart
      integer       (ip       ) ,  pointer, dimension(:       ) :: mpart0
      integer       (ip       ) ,  pointer, dimension(:       ) :: mplsta
      integer       (ip       ) ,  pointer, dimension(:       ) :: mstat
      integer       (ip       ) ,  pointer, dimension(:       ) :: mwaste
      integer       (ip       ) ,  pointer, dimension(:       ) :: npart0
      integer       (ip       ) ,  pointer, dimension(:       ) :: nplsta
      integer       (ip       ) ,  pointer, dimension(:       ) :: nstat
      integer       (ip       ) ,  pointer, dimension(:       ) :: nwaste
      integer       (ip       ) ,  pointer, dimension(:,:     ) :: imap
      integer       (ip       ) ,  pointer, dimension(:,:     ) :: imask
      integer       (ip       ) ,  pointer, dimension(:,:     ) :: ibuff
      integer       (ip       ) ,  pointer, dimension(:,:     ) :: mcell
      integer       (ip       ) ,  pointer, dimension(:,:     ) :: ncell
      integer       (ip       ) ,  pointer, dimension(:,:,:   ) :: nbin
      integer       (ip       ) ,  pointer, dimension(:       ) :: nosyss
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
      integer       (ip       ) ,  pointer, dimension(:       ) :: elt_bytes
      integer       (ip       ) ,  pointer, dimension(:,:     ) :: elt_dims
      real          (sp       ) ,  pointer, dimension(:       ) :: rbuffr
      real          (sp       ) ,  pointer, dimension(:,:     ) :: concp
      real          (sp       ) ,  pointer, dimension(:,:     ) :: flres

      real     ( rp), pointer  :: v_swim(:)    ! horizontal swim velocity (m/s)
      real     ( rp), pointer  :: d_swim(:)    ! horizontal swim direction (degree)
end module partmem

module m_part_regular
      use precision_part       ! single/double precision
      integer       (ip       ) ,  pointer, dimension(:       ) :: npart
      real          (sp       ) ,  pointer, dimension(:       ) :: xpart
      real          (sp       ) ,  pointer, dimension(:       ) :: ypart
      real          (sp       ) ,  pointer, dimension(:       ) :: zpart
      integer       (ip       ) ,  pointer, dimension(:       ) :: kpart
      integer       (ip       ) ,  pointer, dimension(:       ) :: isub

end module m_part_regular

module spec_feat_par

      !     special feature parameters

      use precision_part      ! single and double precision

!     vertical bounce
      logical                                                   :: vertical_bounce

!     wind drag for all particles
      logical                                                   :: apply_wind_drag
      real      (sp)                                            :: max_wind_drag_depth

!     restart files
      logical                                                   :: write_restart_file
      integer  (ip)                                             :: max_restart_age

!     plastics parameters
      integer   (ip)            ,  pointer, dimension(:       ) :: plparset
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
      integer  ( ip)           :: nrowsscreens     ! length of screen polygon
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
