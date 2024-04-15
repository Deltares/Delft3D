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

module abm_mod
!
!  data definition module(s)
!
use m_stop_exit
use m_waq_precision              ! single/double precision
use timers
!
!  module procedure(s)
!
use larvm2_mod                  ! explicit interface
use tidal_state_mod             ! explicit interface

!behavioral modules
use behv_test_mod               ! explicit interface
use behv_atlantic_salmon_mod    ! explicit interface
use behv_european_eel_mod       ! explicit interface
use behv_mauve_stinger_mod      ! explicit interface
use behv_horseshoecrab_mod      ! explicit interface
use behv_mangrove_mod           ! explicit interface
use behv_asiancarpeggs_mod      ! explicit interface


use intpltd_stagedev_mod        ! explicit interface
!
implicit none

contains
      subroutine abm    ( lunrep   , itime    , idelt    , nmax     , mmax     ,    &
                          layt     , nosegl   , nolay    , mnmaxk   , lgrid    ,    &
                          lgrid2   , lgrid3   , nopart   , npwndw   , nosubs   ,    &
                          npart    , mpart    , kpart    , xpart    , ypart    ,    &
                          zpart    , wpart    , iptime   , wsettl   , locdep   ,    &
                          nocons   , const    , conc     , xa       , ya       ,    &
                          angle    , vol1     , vol2     , flow     , depth    ,    &
                          vdiff1   , salin1   , temper1  , v_swim   , d_swim   ,    &
                          itstrtp  , vel1     , vel2     , abmmt    , abmsd    ,    &
                          chronrev , selstage , zmodel   , laybot   , laytop   )

      ! function  : calculates individual based model specifics

      ! arguments :

      integer(int_wp ), intent(in)    :: lunrep              ! report file
      integer(int_wp ), intent(in)    :: itime               ! time in seconds
      integer(int_wp ), intent(in)    :: idelt               ! time step size in seconds
      integer(int_wp ), intent(in)    :: nmax                ! first grid dimension
      integer(int_wp ), intent(in)    :: mmax                ! second grid dimension
      integer(int_wp ), intent(in)    :: layt                ! number of layers of hydr. database
      integer(int_wp ), intent(in)    :: nosegl              ! number segments per layer
      integer(int_wp ), intent(in)    :: nolay               ! number of layers in calculation (all model)
      integer(int_wp )                :: nlay                ! number of layers in calculation (current location)
      integer(int_wp ), intent(in)    :: mnmaxk              ! total number of active grid cells
      integer(int_wp ), intent(in)    :: laytop(nmax,mmax)   !< highest active layer in z-layer model
      integer(int_wp ), intent(in)    :: laybot(nmax,mmax)   !< deepest active layer in z-layer model
      integer(int_wp ), pointer       :: lgrid ( : , : )     ! grid with active grid numbers, negatives for open boundaries
      integer(int_wp ), pointer       :: lgrid2( : , : )     ! total grid
      integer(int_wp ), pointer       :: lgrid3( : , : )     ! original grid (conc array)
      integer(int_wp ), intent(in)    :: nopart              ! total number of particles
      integer(int_wp ), intent(in)    :: npwndw              ! first active particle
      integer(int_wp ), intent(in)    :: nosubs              ! number of substances per particle
      integer(int_wp ), pointer       :: npart ( : )         ! first  grid index of the particles
      integer(int_wp ), pointer       :: mpart ( : )         ! second grid index of the particles
      integer(int_wp ), pointer       :: kpart ( : )         ! third grid index of the particles
      real   (sp), pointer       :: xpart ( : )         ! x-value (0.0-1.0) first  direction within grid cell
      real   (sp), pointer       :: ypart ( : )         ! y-value (0.0-1.0) second direction within grid cell
      real   (sp), pointer       :: zpart ( : )         ! z-value (0.0-1.0) third  direction within grid cell
      real   (sp), pointer       :: wpart ( : , :)      ! weight factors of the subs per particle
      integer(int_wp ), pointer       :: iptime( : )         ! particle age in seconds
      real   (sp), pointer       :: wsettl( : )         ! settling per particle
      real   (sp), pointer       :: locdep( : , : )     ! depth per layer
      real   (sp), pointer       :: depth( : )          ! total depth of the cells from top to bottom
      integer(int_wp ), intent(in)    :: nocons              ! number of constants
      real   (sp), pointer       :: const ( : )         ! user-defined constants
      real   (sp), pointer       :: conc  ( : , : )     ! concentration array in transport grid
      real   (sp), pointer       :: xa    ( : )         ! x-coordiante in real world
      real   (sp), pointer       :: ya    ( : )         ! y-coordinate in real world
      real   (sp), pointer       :: angle ( : )         ! angle with horizontal
      real   (sp), pointer       :: vol1  ( : )         ! volume begin hydr step
      real   (sp), pointer       :: vol2  ( : )         ! volume end hydr step
      real   (sp), pointer       :: flow  ( : )         ! all flows
      real   (sp), pointer       :: vdiff1( : )         ! vert. diffusion segment numbering
      real   (sp), pointer       :: salin1( : )         ! salinity segment numbering
      real   (sp), pointer       :: temper1( : )        ! temperature segment numbering
      real   (sp), pointer       :: v_swim( : )         ! horizontal swimming velocity m/s
      real   (sp), pointer       :: d_swim( : )         ! horizontal swimming direction (degree)
      integer(int_wp ), intent(in)    :: itstrtp             ! start time
      real   (sp), pointer       :: vel1  ( : )         ! velocity begin hydr step
      real   (sp), pointer       :: vel2  ( : )         ! velocity end hydr step

      logical                    :: zmodel              ! indicates zmodel
      logical                    :: debug = .false.     ! indicates debugging mode

      ! from input (const)

      integer, save              :: nstage              ! number of stages
      integer, save              :: iday = 0            ! for csv output
      integer, save              :: ncum = 0            ! for csv output
      character(len=256)              :: filcsv
      integer, save              :: luncsv
      real(sp), pointer,save     :: astage(:)           ! a coefficient in stage development (-)
      real(sp), pointer,save     :: bstage(:)           ! b coefficient in stage development (-)
      integer(int_wp ), pointer,save  :: btype(:)       ! behaviour type
      real(sp), pointer,save     :: mort1(:)            ! base mortality begin stage
      real(sp), pointer,save     :: mort2(:)            ! base mortality end stage
      real(sp), pointer,save     :: tcmort(:)           ! temperature coefficient mortality
      real(sp), pointer,save     :: buoy1(:)            ! buoyancy begin stage
      real(sp), pointer,save     :: buoy2(:)            ! buoyancy end stage
      real(sp), pointer,save     :: vzact1(:)           ! active vertical velocity begin stage
      real(sp), pointer,save     :: vzact2(:)           ! active vertical velocity end stage
      real(sp), pointer,save     :: ztop1(:)            ! depth top layer begin stage
      real(sp), pointer,save     :: ztop2(:)            ! depth top layer end stage
      real(sp), pointer,save     :: zbot1(:)            ! depth bot layer begin stage
      real(sp), pointer,save     :: zbot2(:)            ! depth bot layer end stage
      real(sp), pointer,save     :: phase(:)            ! phase in diurnal behaviour
      integer(sp), pointer,save  :: hbtype(:)           ! horizontal behaviour type
      real(sp), pointer,save     :: vswim1(:)           ! swim velocity at begin of stage
      real(sp), pointer,save     :: vswim2(:)           ! swim velocity at end of stage
      integer, save              :: istage_nursery      ! stage from where larvae can settle in nursery area
      integer, save              :: layer_release       ! layer where eggs are released
      integer, save              :: it_start_m2         ! start time m2 output
      integer, save              :: it_stop_m2          ! stop time m2 output
      integer, save              :: idt_m2              ! timestep m2 output
      integer, save              :: idt_csv             ! timestep csv output, 0 = not, times as m2
      integer, save              :: iniday              ! release of initial condition, not used here
      integer, save              :: tide_opt            ! option in tidal state determination

      ! local :

      real(sp), pointer,save     :: phase_diurn(:)      ! phase in diurnal behaviour
      integer(int_wp )                :: ipart               ! particle index
      real   (sp)                :: stage               ! stage development
      real   (sp)                :: fstage              ! fraction of current stage
      integer(int_wp )                :: istage              ! integer stage development
      integer(int_wp )                :: istage_t            ! index stage development
      integer(int_wp )                :: istag0              ! previous stage
      real   (sp)                :: stime               ! time since last stage change
      real   (sp)                :: stemp               ! average temperature since last stage change
      real   (sp)                :: dtemp               ! average temp day
      real   (sp)                :: ddepth              ! average depth day
      real   (sp)                :: sdepth              ! average depth since last stage change
      real   (sp)                :: a                   ! a coefficient in development (-)
      real   (sp)                :: b                   ! b coefficient in development (-)
      real   (sp)                :: verdiff             ! verticle diffusion (m2/s1)
      real   (sp)                :: temp                ! temperature        (degrees Celsius)
      real   (sp)                :: salinity            ! salinity
      real   (sp)                :: salprev             ! salinity previous
      logical                    :: eb_tide             ! eb_tide
      real   (sp)                :: delt                ! timestep in days
      real   (sp)                :: day                 ! time in days
      real   (sp)                :: duration            ! duration of current stage
      real   (sp)                :: nduration           ! duration of next stage (chron rev modelling)
      real   (sp)                :: oldduration         ! duration of previous time step (chron rev modelling)
      logical, save              :: is_nursery          ! if arived at nursery area
      real   (sp)                :: vz                  ! vz (m/s)
      real   (sp)                :: vzact               ! vzact (m/s)
      real   (sp)                :: buoy                ! buoy (m/s)
      real   (sp)                :: mort                ! mortality
      real   (sp)                :: ztop                ! ztop
      real   (sp)                :: zbot                ! zbot
      integer                    :: m                   ! m
      integer                    :: n                   ! n
      integer                    :: k                   ! k
      integer                    :: kb                  ! k
      integer                    :: kseg                ! k - 1
      integer                    :: ktopp               ! ktopp
      integer                    :: kbotp               ! kbotp
      integer (int_wp )               :: iseg                ! iseg
      integer                    :: isegl               ! isegl
      integer                    :: isegb               ! isegb
      real   (sp)                :: z                   ! z
      real   (sp)                :: zdepth              ! z relative to water surface
      real   (sp)                :: zlevel              ! z relative to bottom
      real   (sp)                :: laydep              ! laydep
      real   (sp)                :: totdep              ! totdep
      real   (sp)                :: volseg              ! volseg
      real   (sp)                :: totvol              ! totvol for m2 output
      real   (sp)                :: totconc             ! total of particles over the watercolumn (for m2 output)
      logical, save              :: l_csv               ! csv output
      logical                    :: l_csv_now           ! csv output at this timestep
      logical                    :: frelease = .false.  ! first release (used for rev chron)
      logical, pointer, save     :: ebb_flow( : )       ! true if flow is ebb

      integer                    :: abmmt                          ! actual model type

      integer, parameter         :: model_none             = 0     ! model type none
      integer, parameter         :: model_european_eel     = 1     ! model type European eel
      integer, parameter         :: model_atlantic_salmon  = 2     ! model type Atlantic salmon
      integer, parameter         :: model_mauve_stinger    = 3     ! model type Mauve stinger
      integer, parameter         :: model_horseshoecrab    = 4     ! model type Horseshoe crab
      integer, parameter         :: model_mangrove_seeds   = 5     ! model type Mangrove propagules
      integer, parameter         :: model_asian_carp_eggs  = 6     ! model type Asian Carp Eggs

      integer                    :: behaviour_type      ! actual behaviour type

      integer, parameter         :: behaviour_none     = 0 ! behaviour type none
      integer, parameter         :: behaviour_bottom   = 1 ! behaviour type bottom
      integer, parameter         :: behaviour_midlow   = 2 ! behaviour type midlow
      integer, parameter         :: behaviour_midtop   = 3 ! behaviour type midtop
      integer, parameter         :: behaviour_pelagic  = 4 ! behaviour type pelagic
      integer, parameter         :: behaviour_demersal = 5 ! behaviour type demersal
      integer, parameter         :: behaviour_diurnal  = 6 ! behaviour type diurnal
      integer, parameter         :: behaviour_herring  = 7 ! behaviour type herring
      integer, parameter         :: behaviour_stst_dem = 8 ! behaviour type stst_dem
      integer, parameter         :: behaviour_stst_pel = 9 ! behaviour type stst_pel

      integer                    :: h_behaviour_type       ! horizontal behaviour

      integer,parameter          :: h_behaviour_none     = 0  ! no horizontal behaviour
      integer,parameter          :: h_behaviour_salinity = 1  ! horizontal behaviour towards lowest salinity
      integer,parameter          :: h_behaviour_temp_sal = 2  ! horizontal behaviour towards lowest salinity
                                                              ! avoinding temperature boundaries

      integer                    :: abmsd                     ! stage development method

      integer,parameter          :: dev_fixed           = 0   ! value based stage development
      integer,parameter          :: dev_linear          = 1   ! linear exponential stage development
      integer,parameter          :: dev_intpltd         = 2   ! interplolated stage development
      integer,parameter          :: dev_asian_carp_egg  = 3   ! asian carp egg stage development

      logical                    :: chronrev                  ! reverse chronological order of ABM model
      real                       :: selstage                  ! stage for release in reversed ABM model

      real                       :: vswim                  ! swimming velocity
      real                       :: low_sal                ! lowest salinity
      integer                    :: sal_n0
      integer                    :: sal_n1
      integer                    :: sal_n12
      integer                    :: sal_n2
      integer                    :: sal_n23
      integer                    :: sal_n3
      integer                    :: sal_n34
      integer                    :: sal_n4
      integer                    :: sal_n41
      real                       :: low_temp            ! lowest temperature
      real                       :: lb_temp             ! lower boundary of temperature
      real                       :: ub_temp             ! upper boundary of temperature
      integer                    :: temp_n0
      integer                    :: temp_n1
      integer                    :: temp_n12
      integer                    :: temp_n2
      integer                    :: temp_n23
      integer                    :: temp_n3
      integer                    :: temp_n34
      integer                    :: temp_n4
      integer                    :: temp_n41
      integer                    :: n_low                  ! segment with lowest salinity
      integer                    :: n0
      integer                    :: n1
      integer                    :: n12
      integer                    :: n2
      integer                    :: n23
      integer                    :: n3
      integer                    :: n34
      integer                    :: n4
      integer                    :: n41
      real                       :: x_low                  ! x lowest salinity
      real                       :: y_low                  ! y lowest salinity
      real                       :: local_angle            ! angle towards lowest salinity in grid
      logical                    :: thd_n1                 ! thin dam towards n1
      logical                    :: thd_n2                 ! thin dam towards n2
      logical                    :: thd_n3                 ! thin dam towards n3
      logical                    :: thd_n4                 ! thin dam towards n4

      real   (sp)                :: dev_factor             ! stage development factor

      logical                    :: stick_to_bottom        ! stick to bottom when reached

      real   , parameter         :: pi = 3.141592654
      real   , parameter         :: twopi = pi*2.0
      integer                    :: nconst                 ! count of constants
      integer                    :: nfix                   ! number of constants
      integer, parameter         :: nfix_std           = 9 ! fixed number of constants (for standard ABM setup)
      integer, parameter         :: nvar               =18 ! variable number of constants per stage

      integer, save              :: ifirst = 1
      logical, save              :: l_larvae
      integer(4),save            :: ithndl = 0             ! handle to time this subroutine

      ! Asian carp model specific parameters
      integer, parameter         :: nfix_ace          = 20 ! fixed number of constants (for asian carp egg ABM setup)
      real , save                :: factor_alpha           ! derived factor to predict species specific egg diameter at 22 degrees Celsius (unitless)
      real , save                :: factor_beta            ! derived factor to predict species specific egg diameter at 22 degrees Celsius (unitless)
      real , save                :: factor_delta           ! derived factor to predict species specific egg density at 22 degrees Celsius (unitless)
      real , save                :: factor_eta             ! derived factor to predict species specific egg density corrected for specific temperature (unitless)
      real , save                :: factor_gamma           ! derived factor to predict species specific egg density at 22 degrees Celsius (unitless)
      real , save                :: factor_phi             ! derived factor to predict species specific egg density at 22 degrees Celsius (unitless)

      ! Asian carp stage development specific parameters
      real , save                :: par_a                  ! parameter A to determine egg hatching time based on temperature
      real , save                :: par_b                  ! parameter B to determine egg hatching time based on temperature
      real , save                :: par_c                  ! parameter C to determine egg hatching time based on temperature
      real , save                :: par_ctum               ! parameter CTUmean to determine larvae gas bladder infalation time based on temperature
      real , save                :: par_tmin               ! parameter Tmin to determine larvae gas bladder infalation time based on temperature

      !Set debugging mode
      ! debug = .true.

      if(debug) write(88,*) 'Time: ', itime

      ! Check if the current time (sec) is equal to start time
      if ( itime .eq. itstrtp ) then

         !If the number of constants are equal to 0 deactivate the larvae model
         if ( nocons .eq. 0 ) then
            write(lunrep,*) 'ERROR no constants, no larvae model activated'
            write(*,*) 'ERROR no constants, no larvae model activated'
            l_larvae = .false.
            stop
         endif

         ! Set the number of stages to the number of ?const? (User defined)
         nstage         = nint(const(1))

         !If these stages are .le. 0 then deactivate larvae model
         if ( nstage .le. 0 ) then
            write(lunrep,*) 'ERROR no stages, no larvae model activated'
            write(*,*) 'ERROR no stages, no larvae model activated'
            l_larvae = .false.
            stop
         endif

         if( abmmt .eq. model_asian_carp_eggs ) then    ! asian carp eggs model
             ! If number of constants .ne. to 9 (fixed number of constants) +
             ! number of stages * 18 (variable number of constants per stage) then
             ! deactivate the larvae model
             if( abmsd .ne. dev_asian_carp_egg ) then
                write(lunrep,*) 'ERROR "dev_asian_carp_egg" is not activated for stage development, no larvae model activated'
                write(*,*) 'ERROR "dev_asian_carp_egg" is not activated for stage development, no larvae model activated'
                l_larvae = .false.
                stop
             endif
             if ( nocons .ne. nfix_ace +nstage*nvar ) then
                write(lunrep,*) 'ERROR no of constants inconsistent with stages of asian carp model, no larvae model activated'
                write(*,*) 'ERROR no of constants inconsistent with stages of asian carp model, no larvae model activated'
                l_larvae = .false.
                stop
             endif

             nfix = nfix_ace

         else

             ! If number of constants .ne. to 9 (fixed number of constants) +
             ! number of stages * 18 (variable number of constants per stage) then
             ! deactivate the larvae model
             if ( nocons .ne. nfix_std +nstage*nvar ) then
                write(lunrep,*) 'ERROR no of constants inconsistent with stages, no larvae model activated'
                write(*,*) 'ERROR no of constants inconsistent with stages, no larvae model activated'
                l_larvae = .false.
                stop
             endif

             nfix = nfix_std

         endif

        !LARVAE MODEL
        ! When the above is not true then the larvae model is activated

        ! First some stages are set
        l_larvae       = .true.
        istage_nursery = nint(const(2))              !Stage that larvae can enter the nursery
        layer_release  = nint(const(3))              !Layer in which the eggs are released

        it_start_m2    = nint(const(4)*86400.)       !Start when concentrations in m2 are outputed
        it_stop_m2     = nint(const(5)*86400.)       !End till when concentrations in m2 are outputed
        idt_m2         = nint(const(6))              !Time step of m2 output
        idt_csv        = nint(const(7))              !Timestep of csv output, 0 indicates not
        iniday         = nint(const(8))              !Release of initial condition
        tide_opt       = nint(const(9))              !Option for tidal state determination

        nconst         = 9                           ! count constants used

        write(*,*) nfix, nvar
        write(*,*) "ABM parameters"
        write(*,*) "istage_nursery : ", istage_nursery
        write(*,*) "layer_release : ", layer_release
        write(*,*) "it_start_m2 : ", it_start_m2
        write(*,*) "it_stop_m2 : ", it_stop_m2
        write(*,*) "idt_m2 : ", idt_m2
        write(*,*) "idt_csv : ", idt_csv
        write(*,*) "iniday : ", iniday
        write(*,*) "tide_opt : ", tide_opt

        if( abmmt .eq. model_asian_carp_eggs .and. abmsd .eq. dev_asian_carp_egg ) then

            ! asian carp eggs model
            factor_alpha       = const(nconst + 1)              !factor_alpha
            factor_beta        = const(nconst + 2)              !factor_beta
            factor_delta       = const(nconst + 3)              !factor_delta
            factor_eta         = const(nconst + 4)              !factor_eta
            factor_gamma       = const(nconst + 5)              !factor_gamma
            factor_phi         = const(nconst + 6)              !factor_phi

            nconst             = nconst + 6                     ! count constants used

            write(*,'(a,f10.5)') "Species parameter Alpha : ", factor_alpha
            write(*,'(a,f10.5)') "Species parameter Beta  : ", factor_beta
            write(*,'(a,f10.5)') "Species parameter Delta : ", factor_delta
            write(*,'(a,f10.5)') "Species parameter Eta   : ", factor_eta
            write(*,'(a,f10.5)') "Species parameter Gamma : ", factor_gamma
            write(*,'(a,f10.5)') "Species parameter Phi   : ", factor_phi

            ! asian carp eggs stage development
            par_a              = const(nconst + 1 )              !par_a stagedev
            par_b              = const(nconst + 2 )              !par_b stagedev
            par_c              = const(nconst + 3 )              !par_c stagedev
            par_ctum           = const(nconst + 4 )              !par_ctum stagedev
            par_tmin           = const(nconst + 5 )              !par_tmin stagedev

            nconst             = nconst + 5                      ! count constants used

            write(*,'(a,f10.5)') "Species stage dev. par. A       : ", par_a
            write(*,'(a,f10.5)') "Species stage dev. par. B       : ", par_b
            write(*,'(a,f10.5)') "Species stage dev. par. C       : ", par_c
            write(*,'(a,f10.5)') "Species stage dev. par. CTUmean : ", par_ctum
            write(*,'(a,f10.5)') "Species stage dev. par. Tmin    : ", par_tmin

        endif

         ! Determine whether csv output of m2 concentrations is required
         if ( idt_csv .le. 0 ) then
            l_csv = .false.
         else
            l_csv = .true.
         endif

         ! Set up the size of the arrays
         allocate(astage(nstage))         !Array for the a coefficient in stage development
         allocate(bstage(nstage))         !Array for the b coefficient in stage development
         allocate(btype(nstage))          !Array for the behaviour type
         allocate(mort1(nstage))          !Array for the mortality in the begin stage (/d)
         allocate(mort2(nstage))          !Array for the mortality in the end stage (/d)
         allocate(tcmort(nstage))         !Array for a temperature based coefficient for mortality (-)
         allocate(buoy1(nstage))          !Array for the buoncy on the begin stage (m/d)
         allocate(buoy2(nstage))          !Array for the buoncy on the end stage (m/d)
         allocate(vzact1(nstage))         !Array for the ?vzact? on the begin stage (m/d)
         allocate(vzact2(nstage))         !Array for the ?vzact? on the end stage (m/d)
         allocate(ztop1(nstage))          !Array for the ?maximum position in the watercolumn? on the begin stage (m)
         allocate(ztop2(nstage))          !Array for the ?maximum position in the watercolumn? on the end stage (m)
         allocate(zbot1(nstage))          !Array for the ?minimum position in the watercolumn? on the begin stage (m)
         allocate(zbot2(nstage))          !Array for the ?minimum position in the watercolumn? on the end stage (m)
         allocate(phase(nstage))          !Array for the phase in diurnal behaviour (d)
         allocate(phase_diurn(nstage))    !Array for the ?phase in diurnal behaviour? (d)
         allocate(hbtype(nstage))         !Array for the horizontal behaviour type (-)
         allocate(vswim1(nstage))         !Array for the swimming velocity on the begin stage (m/s)
         allocate(vswim2(nstage))         !Array for the swimming velocity on the end stage (m/s)

         !Determine the value in the array for the number of stages times
         !the variable number of constants per stage
         write(*,*)
         write(*,*) "Stage parameters"
         do istage = 1, nstage
            astage(istage) = const(nfix+(istage-1)*nvar+ 1)
            bstage(istage) = const(nfix+(istage-1)*nvar+ 2)
            btype(istage)  = const(nfix+(istage-1)*nvar+ 3)
            mort1(istage)  = const(nfix+(istage-1)*nvar+ 4)
            mort2(istage)  = const(nfix+(istage-1)*nvar+ 5)
            tcmort(istage) = const(nfix+(istage-1)*nvar+ 6)
            buoy1(istage)  =-const(nfix+(istage-1)*nvar+ 7)/86400.   !Is converted to m/s
            buoy2(istage)  =-const(nfix+(istage-1)*nvar+ 8)/86400.   !Is converted to m/s
            vzact1(istage) =-const(nfix+(istage-1)*nvar+ 9)/86400.   !Is converted to m/s
            vzact2(istage) =-const(nfix+(istage-1)*nvar+10)/86400.   !Is converted to m/s
            ztop1(istage)  = const(nfix+(istage-1)*nvar+11)
            ztop2(istage)  = const(nfix+(istage-1)*nvar+12)
            zbot1(istage)  = const(nfix+(istage-1)*nvar+13)
            zbot2(istage)  = const(nfix+(istage-1)*nvar+14)
            phase(istage)  = const(nfix+(istage-1)*nvar+15)
            hbtype(istage)  = const(nfix+(istage-1)*nvar+16)
            vswim1(istage)  = const(nfix+(istage-1)*nvar+17)
            vswim2(istage)  = const(nfix+(istage-1)*nvar+18)

            write(*,*)
            write(*,*) "stage nr                : ", istage
            write(*,*) "astage                  : ", astage
            write(*,*) "bstage                  : ", bstage
            write(*,*) "btype                   : ", btype
            write(*,*) "mort1  & mort2 & tcmort : ", mort1, mort2, tcmort
            write(*,*) "buoy1  & buoy2          : ", buoy1 , buoy2
            write(*,*) "vzact1 & vzact2         : ", vzact1 , vzact2
            write(*,*) "ztop1 & ztop2           : ", ztop1 , ztop2
            write(*,*) "zbot1 & zbot2           : ", zbot1 , zbot2
            write(*,*) "phase                   : ", phase
            write(*,*) "hbtype                  : ", hbtype
            write(*,*) "vswim1 & vswim2         : ", vswim1 , vswim2

         enddo

         !Create and array to store the ebb_flow of the modelgrid
         allocate(ebb_flow(nosegl))



         ! set some stuff hard coded for the moment

         is_nursery     = .true.

      endif


      ! exit if no larvae model

      if ( .not. l_larvae ) return

      !For debugging perpuses with timon the time
      !used for handling the subroutine can be recorded
      if ( timon ) call timstrt( "larvae", ithndl )

      ! global forcing

      ! Calculate timestep sizes
      delt     = idelt/86400.    !Timestep size in days
      day      = itime/86400.    !Current number of days

      ! Calculate the phase during the day
      do istage = 1, nstage
         phase_diurn(istage) = sin((day-phase(istage))*twopi)/2.+.5
      enddo

      !Determine the tide for the complete grid
      !Based on the volume and the velocity for all the segments
      !returning true in ebb_flow when it is low tide
      call tidal_state( lunrep, itime, zmodel, itstrtp, nosegl, tide_opt, vol1, vol2, vel1, vel2, ebb_flow)


      !If the situation is met output the amount
      !of larvae per m2 per gridcell
      if ( itime .ge. it_start_m2 .and. itime .le. it_stop_m2 .and. mod(itime-it_start_m2,idt_m2) .lt. idelt ) then
         call larvm2 ( lunrep   , itime    , nosegl   , nolay    , nosubs   ,    &
                       conc     )
      endif

      ! Open an csv file for every day output on the position
      l_csv_now = .false.
      if ( l_csv ) then
         if ( itime .ge. it_start_m2 .and. itime .le. it_stop_m2 .and. mod(itime-it_start_m2,idt_csv) .lt. idelt ) then
            l_csv_now = .true.
            iday = (itime-it_start_m2)/idt_csv
            write(filcsv,'(a,i5.5,a)') 'position_day_',iday,'.csv'
            open(newunit=luncsv,file=filcsv)
         endif
         ncum = ncum + 1
      endif

      ! Loop for the first active particle to all the number of particles
      ! and store the position and forcing of the particle
      do ipart = npwndw, nopart

         m      = mpart(ipart)            !Second grid index of the current particle
         n      = npart(ipart)            !First grid index of the current particle
         k      = min(kpart(ipart),nolay) !Third grid index of k which can not be higher than the number of layers
         kb     = kpart(ipart)            !Third grid index of k (actual position)
         z      = zpart(ipart)            !The z value within the gridcell (0.0 - 1.0)
         iseg   = lgrid3(n,m)             !The original grid number (conc array)

         if(debug) write(88,*) '     iseg, n , m, kb :', iseg , n , m, kb

         if (iseg .gt. 0) then    !If the segment numer is larger than 0 and the particle is within model domain

            ktopp = laytop(n,m)
            kbotp = laybot(n,m)


            isegl  = iseg + (k-1) * nosegl     !Segment number 3d of the particle(segment number + current layer * segments per layer)
            isegb  = iseg + (nolay-1)*nosegl    !Segment number 3d of the particle on the bottom.

            laydep = locdep(lgrid2(n,m),k)                                                !Depth of the layer in which the particle is positioned (is used for substraction)
            if( k .ne. 1 ) laydep = laydep - locdep(lgrid2(n,m),k-1)                      !Distance of particle from the above layer

            nlay = nolay
            if(zmodel) nlay = laybot(n,m)                                                 ! get location specific number of layers when zmodel

            totdep = locdep(lgrid2(n,m),nlay)                                             !The total depth of the gridcell
            zlevel = locdep(lgrid2(n,m),nlay) - locdep(lgrid2(n,m),k) + (1.-z)*laydep     !Depth of particel measured from bottom
            zdepth = locdep(lgrid2(n,m),k) - (1.-z)*laydep                                !Depth of particel measured from surface

            verdiff  = vdiff1(isegl)      !Verticle diffusion of particle segment 3d numbering
            temp     = temper1(isegl)     !Temperature of particle segment 3d numbering
            salinity = salin1(isegl)      !Salinity of particle segment 3d numbering

            if(temp .eq. 0.0 .and. zmodel) then
               !issue in zmodel with temper1, salin1 and vdiff1 due to varying water level in zmodel
                isegl    = iseg + (k) * nosegl
                verdiff  = vdiff1(isegl)      !Verticle diffusion of particle segment 3d numbering
                temp     = temper1(isegl)     !Temperature of particle segment 3d numbering
                salinity = salin1(isegl)      !Salinity of particle segment 3d numbering
            endif

             !Stage development

             stage  = wpart(2,ipart)                                              !Time required to spent in stage
             stime  = wpart(3,ipart)                                              !Time since last stage change
             stemp  = wpart(4,ipart)                                              !Average temperature encountered since last stage change
             if ( nosubs .ge. 4+nstage+1 ) dtemp    = wpart(4+nstage+1,ipart)       !Get sum of all temperatures encountered
             if ( nosubs .ge. 4+nstage+2 ) ddepth   = wpart(4+nstage+2,ipart)       !Get sum of all depths encountered
             if ( nosubs .ge. 4+nstage+3 ) sdepth   = wpart(4+nstage+3,ipart)       !Get averaged depth endured in stage

             istage = stage                                                       !Current stage (as integer)
             istage = max(istage,0)                                               !Set a minimum stage development of 0
             istag0 = istage                                                      !Store as previous stage development

             if(debug) write(88,*) 'Particle (stage): ', ipart, istage, stage, stime

             !Add experienced depth and temperature

             stemp  = (stemp*stime + temp*delt)/(stime+delt)                                     !Averaged temperature endured in stage (including this timestep)
             if ( nosubs .ge. 4+nstage+1 ) dtemp  = dtemp + temp                                 !Add all temperatures encountered to generate the averaged temp later on
             if ( nosubs .ge. 4+nstage+2 ) ddepth = ddepth + zdepth                              !Add all depths encoutered to generate the averaged depth later on
             if ( nosubs .ge. 4+nstage+3 ) sdepth = (sdepth*stime + zdepth*delt)/(stime+delt)    !Averaged depth endured in stage (including this timestep)

             !Determine time spent in stage
             if( .not. chronrev ) then
                stime  = stime + delt                                                                 !Complete time spent in stage (including this timestep) chronilogically
             else
                stime  = stime - delt                                                                 !Complete time spent in stage (including this timestep) reversed chronilogically
             endif

             !Development dependend stage time on temperature

             ! Release stage and stage development

             if ( istage .eq. 0 .and. (.not. chronrev) ) then      !If the stage is 0 and if chronologically modelled
                if ( stime .gt. -(stage+delt)) then    !and the time spend is more than required for stage 0 (including this time step).
                    stage  = 1.0                                  ! Evolve to the stage 1
                    istage = 1
                    fstage = 0.0                                  ! Start at 0 fraction of current stage
                    stime  = 0.0                                  ! Set the time spent in the stage to 0
                    kpart(ipart) = layer_release                  ! Position the particle in the release layer for the Third grid dimension track
                    zpart(ipart) = 0.5
                    k            = layer_release                  ! Position the particle in the release layer for the Third gird layers track
                    kb           = layer_release                  ! Position the particle in the release layer for the Third gird actual layers track
                    duration     = -999.                          ! Set a default value for duration
                endif
             else
                if( istage .eq. 0 .and. chronrev .and. (.not. stime .gt. -(stage+delt))) then  !If the stage is 0 and reversed chronologically modelled
                    frelease = .true.                             !first release of particle in reversed modelled
                    istage = int(selstage)
                    istage = min(istage,5)                        ! Maximum of stages set on 5
                    if(debug) write(88,*) 'Set first stage chronrev: ', istage
                endif
            endif

            !determine the duration that needs to be calculated
            select case ( abmsd )   !If the stage is not 0

                case(dev_fixed)                                                             !Stage development 0
                    a = astage(istage)                               ! Determine the a-coefficient for stage development
                    b = bstage(istage)                               ! Determine the b-coefficient for stage development
                    duration = a+b                                   ! Determine the duration of stage development based on a and b

                case(dev_linear)                                                             !Stage development 1
                    a = astage(istage)                               ! Determine the a-coefficient for stage development
                    b = bstage(istage)                               ! Determine the b-coefficient for stage development
                    duration = exp(a+b*stemp)                        ! Determine the duration of stage development based on a, b and stage experienced temp

                case(dev_intpltd)                                                            !Stage development 2
                    call intpltd_stagedev(lunrep, stemp, dev_factor)
                    duration = astage(istage) + bstage(istage) * (1-dev_factor)

                case(dev_asian_carp_egg)                                                     !Stage development 3
                    if( abmmt .ne. 6 ) then
                        write(lunrep,*) ' error, stage development method for asian carp egg defined, but not set as model'  ! Give an error notice that stage development is not defined
                        call stop_exit(1)                                                         ! Stop the calculation
                    endif
                    if(istage .eq. 1 ) then                                                   ! stage developement for eggs (stage 1)
                        duration  = par_a * (stemp ** par_b) + par_c                          ! hatching time (hr)
                        duration  = duration / 24                                             ! development time (days)
                        nduration = 9999.0                                                    ! no stage developement from here
                        if(debug) write(88,*) 'Dev for stage 1 : ', istage, duration, nduration
                    elseif (istage .eq. 2 ) then                                              ! stage development for larvae without GBI (stage 2)
                        duration  = par_ctum / ( stemp - par_tmin )                           ! Gass bladder Infaltion time (hr)
                        duration  = duration / 24                                             ! development time (days)
                        nduration = par_a * (stemp ** par_b) + par_c                          ! hatching time (hr)
                        nduration = nduration / 24                                             ! development time (days)
                        if(debug) write(88,*) 'Dev for stage 2 : ', istage, duration, nduration
                    else
                        duration  = 9999.0                                                    ! no stage developement from here
                        nduration = 9999.0                                                    ! no stage developement from here
                        if(debug) write(88,*) 'Dev for stage other : ', istage, duration, nduration
                    endif


                case default                                                                 !Stage development Default

                    write(lunrep,*) ' error, stage development method type not defined'       ! Give an error notice that stage development is not defined
                    call stop_exit(1)                                                         ! Stop the calculation

            end select

            if( istage .eq. int(selstage) .and. frelease .and. chronrev ) then      !If the stage is 0 and reversed chronologically modelled
                fstage       = min((selstage - REAL(INT(selstage))),0.9999)          ! Set the current development fase of stage (can't be higher than 1)
                stime        = fstage * duration              ! Set the time spent in the stage to 0
                stage        = REAL(istage) + fstage          ! Set the real stage
                kpart(ipart) = layer_release                  ! Position the particle in the release layer for the Third grid dimension track
                zpart(ipart) = 0.5
                k            = layer_release                  ! Position the particle in the release layer for the Third gird layers track
                kb           = layer_release                  ! Position the particle in the release layer for the Third gird actual layers track
                if(debug) write(88,*) 'First release :', fstage, stage, k
            endif

            if( .not. chronrev) then
                if ( duration .lt. stime ) then       !Go to the next stage when this duration is lower than the time spent in the stage
                    istage = istage + 1                           ! Stage + 1
                    istage = min(istage,5)                        ! Maximum of stages set on 5
                    stime  = 0.0                                  ! Restart the stage timer
                endif
            else
                fstage = min((stage - REAL(INT(stage))),0.9999)   !Calculate fstage of previous timestep
                if( frelease) then
                    oldduration = stime / fstage                  ! Calculate the previous duration time
                    frelease = .false.
                    if(debug) write(88,*) 'First release set to false', frelease, oldduration
                else
                    oldduration = (stime + delt)/fstage          ! Calculate the previous duration time
                    if(debug) write(88,*) 'Calc old duration', oldduration
                endif

                if(0.0 .gt. stime) then                           ! if next stage reached
                    if(debug) write(88,*) '     stime, istage, duration before new stage:', stime, istage, duration, nduration, oldduration

                    istage = istage - 1                           ! Stage - 1
                    istage = max(istage,0)                        ! Minimum of stages set on 0
                    stime  = nduration * 0.9999                   ! Set the stage timer to new duration
                    duration = nduration                          ! Set the new duration

                    if(debug) write(88,*) '     stime, istage, duration after new stage:', stime, istage, duration, nduration, oldduration
                else
                    stime = stime + (duration - oldduration)       ! Correct development time for new duration
                endif
            endif
            fstage = stime / duration                        ! Determine the fraction of stage
            fstage = max(fstage,0.0)                         ! Make sure fstage does not become negative
            stage  = istage + fstage                         ! Calculate the current stage (as real)

            if(debug) write(88,*) '     stage set to:', stage

            wpart(2,ipart) = stage                              !Store the stage
            wpart(3,ipart) = stime                              !Store the time spent in stage
            wpart(4,ipart) = stemp                              !Store the average temperature experienced in stage


            !Mortality

            if ( istage .gt. 0 ) then                                               !If the particles are released
                mort = mort1(istage) + fstage*(mort2(istage)-mort1(istage))          ! Determine the mortality coefficient based on the stage
                if ( abs(tcmort(istage)) .gt. 1.e-20 ) then                          !If the temperature based mortality is greater than 0
                    mort = mort*exp(tcmort(istage)*temp)                              ! Add temperature mortality to the overall mortality coefficient
                endif
                wpart(1,ipart) = wpart(1,ipart) - mort*wpart(1,ipart)*delt           !Substract the mortality from the number of individuals and store the result
            endif

            ! if there is substances per fraction set weight according to stage

            do istage_t = 1, nstage                                                 !Loop for number of stages
                if ( istage_t+4 .le. nosubs ) then                                   !?If stage index + 4 is .le. than number of substances then? (+4 is reference to wpart storage)
                    if ( istage_t .eq. istage ) then                                  !If stage index is equal to current stage
                        wpart(istage_t+4,ipart) = wpart(1,ipart)                       ! Transfer the amount of individuals towards the extra substance storage in wpart
                    else
                        wpart(istage_t+4,ipart) = 0.0                                  ! Store 0.0 if the current stage is not equal to loop stage
                    endif
                endif
            enddo


            !Deactivate particles in nursery stage who have arrived in nursery area

            if ( istage .ge. istage_nursery .and. is_nursery .and. .not. chronrev) then !If the stage of nursery is reached and nursery is true
                kpart(ipart) = nolay +1                                              ! Add the particles to the storage layer (total layers + 1)
            elseif ( istage .gt. 0 .or. chronrev ) then                             !If nursery is not reached and particles are active

            !Release any particle from bottom

            if ( kb .gt. nolay ) then                                            !If the layer the particle is positioned is higher than the number of layers
                kpart(ipart) = nolay                                              ! Place the particle in the layer above the bottom for the third dimension track
                k            = nolay                                              ! Place the particle in the layer above the bottom for the third dimension layers
                kb           = nolay                                              ! Place the particle in the layer above the bottom for the third dimension actual position
                zpart(ipart) = 0.5                                                ! Place the particle in the middle of the cell in the third dimension
            endif


            ! Species specific subroutine
            ! SPECIFIC BEHAVIOR ROUTINE SETTING, CALL behv_atlantic_salmon, behv_european_eel, behv_mauve_stinger,
            ! behv_mangrove FOR OTHER CONFIGURATIONS
            ! ADDAPTION IN THE CODE NEEDED

            select case ( abmmt )

                case(model_none)

                    if(chronrev) then
                        write(lunrep,*) ' error, no revere modelling implemented for "behv_test"'       ! Give an error notice that reversed modelling is not implemented
                        call stop_exit(1)                                                               ! Stop the calculation
                    endif

                    call behv_test ( btype    , hbtype  , v_swim , d_swim   , n        ,   &
                                    m        , nmax    , mmax   , mnmaxk , lgrid       ,   &
                                    lgrid2   , lgrid3  , nosegl , wpart  , ipart       ,   &
                                    wsettl   , k       , kpart  , zpart  , xpart       ,   &
                                    ypart    , nolay   , idelt  , day    , phase_diurn ,   &
                                    ebb_flow , flow    , depth  , salin1 , temper1     ,   &
                                    vol1     , vol2    , vel1   , vel2   , fstage      ,   &
                                    ztop     , zlevel  , zdepth , zbot   , buoy        ,   &
                                    vzact    , vz      , buoy1  , buoy2  , ztop1       ,   &
                                    ztop2    , zbot1   , zbot2  , vzact1 , vzact2      ,   &
                                    vswim1   , vswim2  , iseg   , lunrep , angle  )

                case (model_european_eel)

                    if(chronrev) then
                        write(lunrep,*) ' error, no revere modelling implemented for "model_european_eel"'       ! Give an error notice that reversed modelling is not implemented
                        call stop_exit(1)                                                                        ! Stop the calculation
                    endif

                    call behv_european_eel ( btype    , hbtype  , v_swim , d_swim   , n        ,   &
                                    m        , nmax    , mmax   , mnmaxk , lgrid       ,   &
                                    lgrid2   , lgrid3  , nosegl , wpart  , ipart       ,   &
                                    wsettl   , k       , kpart  , zpart  , xpart       ,   &
                                    ypart    , nolay   , idelt  , day    , phase_diurn ,   &
                                    ebb_flow , flow    , depth  , salin1 , temper1     ,   &
                                    vol1     , vol2    , vel1   , vel2   , fstage      ,   &
                                    ztop     , zlevel  , zdepth , zbot   , buoy        ,   &
                                    vzact    , vz      , buoy1  , buoy2  , ztop1       ,   &
                                    ztop2    , zbot1   , zbot2  , vzact1 , vzact2      ,   &
                                    vswim1   , vswim2  , iseg   , lunrep , angle  )

                case (model_atlantic_salmon)

                    if(chronrev) then
                        write(lunrep,*) ' error, no revere modelling implemented for "model_atlantic_salmon"'    ! Give an error notice that reversed modelling is not implemented
                        call stop_exit(1)                                                                        ! Stop the calculation
                    endif

                    call behv_atlantic_salmon ( btype    , hbtype  , v_swim , d_swim   , n    ,   &
                                    m        , nmax    , mmax   , mnmaxk , lgrid       ,   &
                                    lgrid2   , lgrid3  , nosegl , wpart  , ipart       ,   &
                                    wsettl   , k       , kpart  , zpart  , xpart       ,   &
                                    ypart    , nolay   , idelt  , day    , phase_diurn ,   &
                                    ebb_flow , flow    , depth  , salin1 , temper1     ,   &
                                    vol1     , vol2    , vel1   , vel2   , fstage      ,   &
                                    ztop     , zlevel  , zdepth , zbot   , buoy        ,   &
                                    vzact    , vz      , buoy1  , buoy2  , ztop1       ,   &
                                    ztop2    , zbot1   , zbot2  , vzact1 , vzact2      ,   &
                                    vswim1   , vswim2  , iseg   , lunrep , angle  )

                case (model_mauve_stinger)

                    if(chronrev) then
                        write(lunrep,*) ' error, no revere modelling implemented for "model_mauve_stinger"'      ! Give an error notice that reversed modelling is not implemented
                        call stop_exit(1)                                                                        ! Stop the calculation
                    endif

                    call behv_mauve_stinger ( btype    , hbtype  , v_swim , d_swim   , n    ,   &
                                    m        , nmax    , mmax   , mnmaxk , lgrid       ,   &
                                    lgrid2   , lgrid3  , nosegl , wpart  , ipart       ,   &
                                    wsettl   , k       , kpart  , zpart  , xpart       ,   &
                                    ypart    , nolay   , idelt  , day    , phase_diurn ,   &
                                    ebb_flow , flow    , depth  , salin1 , temper1     ,   &
                                    vol1     , vol2    , vel1   , vel2   , fstage      ,   &
                                    ztop     , zlevel  , zdepth , zbot   , buoy        ,   &
                                    vzact    , vz      , buoy1  , buoy2  , ztop1       ,   &
                                    ztop2    , zbot1   , zbot2  , vzact1 , vzact2      ,   &
                                    vswim1   , vswim2  , iseg   , lunrep , angle  )

                case (model_horseshoecrab)

                    if(chronrev) then
                        write(lunrep,*) ' error, no revere modelling implemented for "model_horseshoecrab"'      ! Give an error notice that reversed modelling is not implemented
                        call stop_exit(1)                                                                        ! Stop the calculation
                    endif

                    call behv_horseshoecrab ( btype    , hbtype  , v_swim , d_swim   , n        ,   &
                                    m        , nmax    , mmax   , mnmaxk , lgrid       ,   &
                                    lgrid2   , lgrid3  , nosegl , wpart  , ipart       ,   &
                                    wsettl   , k       , kpart  , zpart  , xpart       ,   &
                                    ypart    , nolay   , idelt  , day    , phase_diurn ,   &
                                    ebb_flow , flow    , depth  , salin1 , temper1     ,   &
                                    vol1     , vol2    , vel1   , vel2   , fstage      ,   &
                                    ztop     , zlevel  , zdepth , zbot   , buoy        ,   &
                                    vzact    , vz      , buoy1  , buoy2  , ztop1       ,   &
                                    ztop2    , zbot1   , zbot2  , vzact1 , vzact2      ,   &
                                    vswim1   , vswim2  , iseg   , lunrep , angle  )

                case (model_mangrove_seeds)

                    if(chronrev) then
                        write(lunrep,*) ' error, no revere modelling implemented for "model_mangrove_seeds"'     ! Give an error notice that reversed modelling is not implemented
                        call stop_exit(1)                                                                        ! Stop the calculation
                    endif

                    call behv_mangrove ( btype    , hbtype  , v_swim , d_swim   , n        ,   &
                                    m        , nmax    , mmax   , mnmaxk , lgrid       ,   &
                                    lgrid2   , lgrid3  , nosegl , wpart  , ipart       ,   &
                                    wsettl   , k       , kpart  , zpart  , xpart       ,   &
                                    ypart    , nolay   , idelt  , day    , phase_diurn ,   &
                                    ebb_flow , flow    , depth  , salin1 , temper1     ,   &
                                    vol1     , vol2    , vel1   , vel2   , fstage      ,   &
                                    ztop     , zlevel  , zdepth , zbot   , buoy        ,   &
                                    vzact    , vz      , buoy1  , buoy2  , ztop1       ,   &
                                    ztop2    , zbot1   , zbot2  , vzact1 , vzact2      ,   &
                                    vswim1   , vswim2  , iseg   , lunrep , angle  )

                case (model_asian_carp_eggs)

                    call behv_asiancarpeggs ( btype    , hbtype  , v_swim , d_swim   , n        ,   &
                                        m        , nmax    , mmax   , mnmaxk , lgrid       ,   &
                                        lgrid2   , lgrid3  , nosegl , wpart  , ipart       ,   &
                                        wsettl   , k       , kpart  , zpart  , xpart       ,   &
                                        ypart    , nolay   , &
                                        ktopp    , kbotp   , idelt  , day    , phase_diurn ,   &
                                        ebb_flow , flow    , depth  , vdiff1 , salin1      ,   &
                                        temper1  , vol1    , vol2   , vel1   , vel2        ,   &
                                        fstage   , ztop    , zlevel , zdepth , zbot        ,   &
                                        buoy     , vzact   , vz     , buoy1  , buoy2       ,   &
                                        ztop1    , ztop2   , zbot1  , zbot2  , vzact1      ,   &
                                        vzact2   , vswim1  , vswim2 , iseg   , lunrep      ,   &
                                        angle, factor_alpha, factor_beta, factor_delta     ,   &
                                        factor_eta, factor_gamma, factor_phi    )


                case default                                                                  !ABM model Default

                    write(lunrep,*) ' error, ABM model type not defined'                      ! Give an error notice that vertical behaviour is not defined
                    call stop_exit(1)                                                         ! Stop the calculation


            end select

            endif


            !reverse the movement direction for the particles
            if(chronrev) then
                wsettl(ipart) = - wsettl(ipart)
                v_swim(ipart) = - v_swim(ipart)
                vzact         = - vzact
                !d_swim stays same velocity of swim is reversed
            endif

            ! every day output position to csv file

            if ( l_csv_now ) then                                                                                       !If csv output on position is activated
                ddepth = ddepth/ncum                                                                                     ! Calculate the depth encountered (only under l_csv_now)
                dtemp  = dtemp/ncum                                                                                      ! Calculate the temp encountered (only under l_csv_now)
                if(dtemp .lt. -40.0 .or. dtemp .eq. 0.) then                                                                                  ! check if model without temperature
                    write(luncsv,'(f10.2,4('','',f10.2),5('','',f10.4),1('','',i5))') xa(ipart),ya(ipart), &             ! Write the x coord, y coord(both real world), depth from
                    zdepth,sdepth,zlevel,-999.,-999.,0., 0., 0.,istage                                                   ! bottom (m), averaged stage depth, depth, temperature, averaged  stage temp, temperature
                                                                                                                            ! time req for next stage, time spent near mortality, fall velo, settling velo., stage nr
                else
                    write(luncsv,'(f10.2,4('','',f10.2),5('','',f10.4),1('','',i5))') xa(ipart),ya(ipart), &             ! Write the x coord, y coord(both real world), depth from
                        zdepth,sdepth,zlevel,stemp,dtemp,duration, vzact, wsettl(ipart),istage                           ! bottom (m), averaged stage depth, depth, temperature, averaged  stage temp, temperature
                                                                                                                            ! time req for next stage, time spent near mortality, fall velo, settling velo., stage nr
                endif
                ddepth = 0.0                                                                                             ! Set ddepth to 0.0 (only under l_csv_now)
                dtemp  = 0.0                                                                                             ! Set dtemp to 0.0 (only under l_csv_now)

            endif

            if ( nosubs .ge. 4+nstage+1 ) wpart(4+nstage+1,ipart) = dtemp                                               ! Store dtemp
            if ( nosubs .ge. 4+nstage+2 ) wpart(4+nstage+2,ipart) = ddepth                                              ! Store ddepth
            if ( nosubs .ge. 4+nstage+3 ) wpart(4+nstage+3,ipart) = sdepth                                              ! Store sdepth

            if(debug) write(88,*) '     new stage:', istage

         else                                                                                                        !If particle is not active
            ! inactive

            if(debug) write(88,*) '     particle inactive, current stage:', istage
            if ( l_csv_now ) then                                                                                       !If csv output on position is activated
                zdepth = -999.                                                                                           ! Store missing for zdepth
                sdepth = -999.                                                                                           ! Store missing for sdepth
                ddepth = -999.                                                                                           ! Store missing for ddepth
                zlevel = -999.                                                                                           ! Store missing for zlevel
                stemp  = -999.                                                                                           ! Store missing for stemp
                dtemp  = -999.
                duration = -999.
                vzact    = 0.0
                istage = 0

                ! Store missing for dtemp
                write(luncsv,'(f10.2,4('','',f10.2),5('','',f10.4),1('','',i5))') xa(ipart),ya(ipart), &                 ! Write the x coord, y coord(both real world), depth from
                        zdepth,sdepth,zlevel,stemp,dtemp,duration, vzact , wsettl(ipart),istage                          ! bottom (m), averaged stage depth, depth, temperature, averaged  stage temp, temperature
                                                                                                                         ! time req for next stage, time spent near mortality, fall velo, settling velo., stage nr
            endif

            if(debug) write(88,*) '     particle inactive:', istage

        endif

        !

      enddo


    ! every day output position to csv file

    if ( l_csv_now ) then                                                                                          !If csv output on position is activated
        close(luncsv)                                                                                               ! Close the output file
        ncum = 0                                                                                                    ! Set the ncum to 0
    endif

    continue

    if ( timon ) call timstop ( ithndl )                                                                           !If the subroutine timer is on stop timing and save time spent
    return                                                                                                         !Return from the subroutine
    end subroutine
end module
