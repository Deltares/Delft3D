module swan_input
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
! WAVE-GUI version number dependencies:
! 4.87.00 (Older than)             : not supported
!
!
!         NOTHING
!         ......
!         NOTHING
!         ......
!         * Level of test output, debug level, Y/N compute waves
!         0 0 1
! 4.88.08 (Newer than or equal to) :
!         * Reflection (0/1), specular or diffuse (1/2), reflection coefficient [0-1]
!         0 1  0.0000000e+000
!         ......
!         * Diffraction, smoothing coefficient, smoothing steps, adaptation of propagation
!         *   - interactions: 0 = de-activated, 1 = activated
!         1  0.2 1 1
!         ......
!         * Level of test output, debug level, Y/N compute waves, Y/N activate hotstart file
!         0 0 1 0
!
!
!         * Number of tidal time points
!         39
! 4.89.05 (Newer than or equal to) :
!         * Number of tidal time points, Reference date
!         39 2006-02-07
!
!
!         * Water level correction
!         0.0000000e+000
! 4.90.00 (Newer than or equal to) :
!         * Water level correction, Extrapolate flow data on the last # grid(s)
!         0.0000000e+000 0
!
!
!         * Y/N Use bathmetry, use waterlevel, use current
!         0 0 0
!         ......
!         * Water level correction, Extend flow data on the last # grid(s),
!         0.0000000e+000 1
!         ......
!         * Level of test output, debug level, Y/N compute waves, Y/N activate hotstart file
!         0 0 1 0
! 4.90.06 (Newer than or equal to) :
!         * Y/N Use bathmetry, use waterlevel, use current, use wind
!         0 0 0 0
!         ......
!         * Water level correction, Extend flow data on the last # grid(s),
!         * Extend bathymetry, water level, current, wind
!         0.0000000e+000 1 1 0 0 0
!         ......
!         * Level of test output, debug level, Y/N compute waves, Y/N activate hotstart file
!         * Output time interval, Computational mode: 0 = stationary, 1 = non-stationary
!         * Non-stationary interval, timestep
!         0 0 1 0  2.2000000e+001 0 6.00000+001 2.00000+000
!
!
!         '40.01'
! 4.91.00 (Newer than or equal to) :
!         NOTHING
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use wave_data
   use handles
   use table_handles
   !
   integer, parameter :: SWAN_MODE_EXE = 0
   integer, parameter :: SWAN_MODE_LIB = 1
   !
   type swan_dom
      real :: freqmax ! maximum frequency
      real :: freqmin ! minimum frequency
      real :: enddir ! end direction for sector
      real :: startdir ! start direction for sector
      real :: veg_height ! vegetation height per layer
      real :: veg_diamtr ! vegetation diameter
      real :: veg_drag ! vegetation drag coefficient
      integer :: veg_nstems ! the number of plant stands per square meter
      integer :: curvibot
      integer :: dirspace ! 1: circle, 2: sector
      integer :: ndir ! number of directional bins
      integer :: nfreq ! number of frequency bins
      integer :: nestnr
      integer :: n_meteofiles ! number of meteo input files
      integer :: n_extforces ! number of external forcing files
      integer :: mxb
      integer :: myb
      integer :: mxc
      integer :: myc
      integer :: vegetation
      integer :: ice = 0
      integer, dimension(5) :: qextnd ! 0: not used, 1: used and not extended, 2: used and extended
      integer :: flowVelocityType = FVT_DEPTH_AVERAGED
      ! Possible values:
      !    FVT_SURFACE_LAYER           : use FLOW velocity at surface
      !    FVT_DEPTH_AVERAGED (default): use depth averaged FLOW velocity
      !    FVT_WAVE_DEPENDENT          : use FLOW velocity, averaged in a wave dependent way
      logical :: cgnum
      character(256) :: botfil
      character(256) :: curlif
      character(256) :: depfil
      character(256) :: nesfil
      character(37) :: vegfil
      character(20) :: nesnam ! dummy
      character(80), dimension(:), allocatable :: extforce_names
   end type swan_dom
   !
   type swan_bnd
      integer :: parread ! 1 = from-file, 2 = parametric
      integer :: sshape ! 1 = Jonswap, 2 = Pierson-Moskowitz, 3 = Gauss
      integer :: periodtype ! 1 = Peak, 2 = Mean
      integer :: dsprtype ! 1 = Power, 2 = Degrees
      integer :: bndtyp ! 1 = orientation, 2 = grid-coordinates, 3 = xy-coordinates
      integer :: orient ! 1 = N, 2 = NW, 3 = W, 4 = SW, 5 = S, 6 = SE, 7 = E, 8 = NE
      integer :: turn ! 0 = clockwise, 1 = counterclockwise (distance measurement along boundary)
      integer :: convar ! 1 = uniform, 2 = space-varying
      integer :: nsect ! previously swani(iindx+9)
      integer, dimension(4) :: bndcrd_mn
      real, dimension(4) :: bndcrd_xy
      real :: gamma0
      real :: sigfr
      !
      integer, dimension(4) :: ts_hs
      integer, dimension(4) :: ts_tp
      integer, dimension(4) :: ts_wd
      integer, dimension(4) :: ts_ds
      !
      real, dimension(:), pointer :: distance
      real, dimension(:), pointer :: waveheight
      real, dimension(:), pointer :: period
      real, dimension(:), pointer :: direction
      real, dimension(:), pointer :: dirspread
      character(20) :: name
      character(37), dimension(:), pointer :: spectrum
   end type swan_bnd
   !
   type swan_type
      integer :: maxbound
      integer :: maxcurv
      integer :: maxnest
      integer :: maxobst
      integer :: maxpoints
      integer, dimension(:), pointer :: maxsect
      integer :: maxsteps
      !
      integer :: diffraction
      integer :: diffr_smsteps
      integer :: diffr_adapt_propag
      integer :: error
      integer :: exemode
      integer :: frictype
      integer :: genmode
      integer :: inrhog
      integer :: icedamp ! 0: off, 1: clip in D-Waves, 2: via SWAN
      integer :: itermx
      integer :: itest
      integer :: itrace
      integer :: modsim ! 0: stationary, 2: non-stationary input, 3: non-stationary input and calculation
      ! modsim = 1 may not be used: is replaced by hotfile
      ! stationary: modsim is set to 2 (0 is not used anymore)
      integer :: mxr
      integer :: mxw
      integer :: myr
      integer :: myw
      integer :: nbound
      integer :: ncrp
      integer :: ncrv
      integer :: ncurv
      integer :: ndec
      integer :: n_meteofiles_gen
      integer :: n_extforces_gen
      integer :: nnest
      integer :: nobst
      integer :: npoints
      integer :: nscr
      integer :: nttide
      integer :: refjulday
      integer :: num_scheme ! Numerical scheme 1: default (S&L for non-stat, SORDUP for stat), 2: BSBT
      integer :: whitecap ! 0: off, 1: on, 2: westhuysen
      integer :: nloc
      integer :: swdis
      integer :: msurpnts ! minimum number of surrounding valid source-points for a target-point to be covered. default: 3, Delft3D: 4
      integer :: output_ice
      integer :: output_veg
      !
      integer, dimension(4) :: ts_wl
      integer, dimension(4) :: ts_xv
      integer, dimension(4) :: ts_yv
      integer, dimension(4) :: ts_ws
      integer, dimension(4) :: ts_wd
      !
      integer, dimension(:), pointer :: reflection
      integer, dimension(:), pointer :: refl_type ! 1: specular, 2: diffuse
      integer, dimension(:), pointer :: nclin
      integer, dimension(:), pointer :: nlin
      !
      logical :: append_com
      logical :: breaking
      logical :: checkVersionNumber = .true.
      logical :: compmode
      logical :: corht
      logical :: curviwind
      logical :: fshift
      logical :: hotfile
      logical :: nautconv
      logical :: output_points
      logical :: output_pnt_file
      logical :: output_spec1d
      logical :: output_spec2d
      logical :: output_table
      logical :: quadruplets
      logical :: refraction
      logical :: setup
      logical :: sferic
      logical :: swflux
      logical :: swmor
      logical :: swuvi
      logical :: swuvt
      logical :: swveg
      logical :: swwindt
      logical :: swwav
      logical :: swwlt
      logical :: swmapwritenetcdf
      logical :: netcdf_sp
      logical :: timedependent
      logical :: triads
      logical :: useflowdata ! true when FLOW data is used
      logical :: varwin
      logical :: varfri
      logical :: windgrowth
      logical :: flowLinkConnectivity ! false: (default) use netlink connectivity from DFlowFM, true: use flowlink connectivity from DFlowFM
      logical :: keepinput
      !
      real :: alpw
      real :: alfa
      real :: cdd
      real :: cfbr1
      real :: cfbr2
      real :: cftriad1
      real :: cftriad2
      real :: css
      real :: deltc ! used when modsim = 3: Time step in non-stat SWAN runs
      real :: nonstat_interval ! used when modsim = 3: Interval of non-stat SWAN computation
      real :: deltcom ! Not used: COM write interval
      real :: inthotf
      real :: depmin
      real :: dh_abs
      real :: diffr_coeff
      real :: drel
      real :: dt_abs
      real :: dxw
      real :: dyw
      real :: excval
      real :: frcof
      real :: gamma0 ! Default gamma0, having a realistic value even if no boundaries are modelled. If boundaries are present gamma0 =  bnd(1)%gamma0
      real :: grav
      real, dimension(7) :: icecoeff
      real :: icewind
      real :: northdir
      real :: percwet
      real :: rho
      real :: rhomud
      real :: tzone !> Time zone for communicating to external forcing module
      real :: viscmud
      real :: xw
      real :: yw
      real :: wavm_write_interval
      real :: int2keephotfile
      real :: veg_height
      real :: veg_diamtr
      integer :: veg_nstems ! the number of plant stands per square meter
      integer :: maxerr = 2 ! Corresponds to maxerr in SWAN: maximum level of errors with which the calculation will continue
      real :: veg_drag
      !
      real :: wlevelcorr ! Overall water level correction; see Time frame input in GUI
      real :: alfawind ! Overall wind speed multiplication factor;
      real, dimension(:), pointer :: timwav
      real, dimension(:), pointer :: zeta ! Default water level of a selected time point (when running stand-alone); see Time frame input in GUI
      real, dimension(:), pointer :: ux0
      real, dimension(:), pointer :: uy0
      real, dimension(:), pointer :: wdir
      real, dimension(:), pointer :: wvel
      !
      real, dimension(:), pointer :: f
      real, dimension(:), pointer :: obet
      real, dimension(:), pointer :: ogam
      real, dimension(:), pointer :: refl_coeff
      real, dimension(:), pointer :: trane
      real, dimension(:), pointer :: xpcu
      real, dimension(:), pointer :: xpob
      real, dimension(:), pointer :: ypcu
      real, dimension(:), pointer :: ypob
      real, dimension(:, :), pointer :: xyloc
      !
      character(4) :: prnumb
      character(7), dimension(:), allocatable :: add_out_names
      character(80), dimension(:), allocatable :: extforce_names_gen
      character(20) :: versionNumberOK = '40.51a' ! No capitals!!!
      character(16) :: prname
      character(37) :: rgfout
      character(37) :: wfil
      character(37) :: ffil
      character(37) :: curvefil
      character(37) :: pntfil
      character(72) :: title1
      character(72) :: title2
      character(72) :: title3
      character(256) :: casl
      character(256) :: filnam
      character(256) :: flowgridfile ! netcdf file containing flow grid
      character(256) :: scriptname
      character(256) :: specfile
      character(256) :: inputtemplatefile
      character(1024) :: comfile
      character(15) :: usehottime = '00000000.000000' ! Time in the name of the hotfile that has to be used by SWAN
      character(15) :: writehottime = '00000000.000000' ! Time in the name of the hotfile that has to be written by SWAN
      character(15) :: keephottime = '00000000.000000' ! Time in the name of the hotfile that should not be deleted
      character(50), dimension(:), allocatable :: pntfilnam ! Name of file containing locations for which output is requested
      character(50), dimension(:), allocatable :: pntfilnamtab ! Name of file containing output on locations
      !
      type(handletype) :: tseriesfile
      !
      type(swan_bnd), dimension(:), pointer :: bnd
      type(swan_dom), dimension(:), pointer :: dom
   end type swan_type
   !
   type(swan_type), save :: swan_run
   !
   integer, parameter :: q_bath = 1 ! used as index in array qextnd
   integer, parameter :: q_wl = 2 ! used as index in array qextnd
   integer, parameter :: q_cur = 3 ! used as index in array qextnd
   integer, parameter :: q_wind = 4 ! used as index in array qextnd
   integer, parameter :: q_veg = 5 ! used as index in array qextnd

   private :: get_pointname

contains
!
!
!==============================================================================
   subroutine dealloc_swan(sr)
      implicit none
      !
      type(swan_type) :: sr
      integer :: i
      integer :: istat
      !
      if (associated(sr%timwav)) deallocate (sr%timwav, stat=istat)
      if (associated(sr%zeta)) deallocate (sr%zeta, stat=istat)
      if (associated(sr%ux0)) deallocate (sr%ux0, stat=istat)
      if (associated(sr%uy0)) deallocate (sr%uy0, stat=istat)
      if (associated(sr%wvel)) deallocate (sr%wvel, stat=istat)
      if (associated(sr%wdir)) deallocate (sr%wdir, stat=istat)
   !!
      if (associated(sr%nclin)) deallocate (sr%nclin, stat=istat)
      if (associated(sr%nlin)) deallocate (sr%nlin, stat=istat)
      if (associated(sr%f)) deallocate (sr%f, stat=istat)
      if (associated(sr%obet)) deallocate (sr%obet, stat=istat)
      if (associated(sr%ogam)) deallocate (sr%ogam, stat=istat)
      if (associated(sr%trane)) deallocate (sr%trane, stat=istat)
      if (associated(sr%xpcu)) deallocate (sr%xpcu, stat=istat)
      if (associated(sr%xpob)) deallocate (sr%xpob, stat=istat)
      if (associated(sr%ypcu)) deallocate (sr%ypcu, stat=istat)
      if (associated(sr%ypob)) deallocate (sr%ypob, stat=istat)
   !!
   !! Only allocate the array below if output to locations has been defined
   !! in the mdw file
   !!
      !if (sr%output_points .and. .not. sr%output_pnt_file) &
      if (associated(sr%xyloc)) deallocate (sr%xyloc, stat=istat)
      !
      if (associated(sr%reflection)) deallocate (sr%reflection, stat=istat)
      if (associated(sr%refl_type)) deallocate (sr%refl_type, stat=istat)
      if (associated(sr%refl_coeff)) deallocate (sr%refl_coeff, stat=istat)
      if (associated(sr%bnd)) then
         do i = 1, sr%maxbound
            if (associated(sr%bnd(i)%distance)) deallocate (sr%bnd(i)%distance, stat=istat)
            if (associated(sr%bnd(i)%waveheight)) deallocate (sr%bnd(i)%waveheight, stat=istat)
            if (associated(sr%bnd(i)%period)) deallocate (sr%bnd(i)%period, stat=istat)
            if (associated(sr%bnd(i)%direction)) deallocate (sr%bnd(i)%direction, stat=istat)
            if (associated(sr%bnd(i)%dirspread)) deallocate (sr%bnd(i)%dirspread, stat=istat)
            if (associated(sr%bnd(i)%spectrum)) deallocate (sr%bnd(i)%spectrum, stat=istat)
         end do
         deallocate (sr%bnd, stat=istat)
      end if
      if (associated(sr%dom)) deallocate (sr%dom, stat=istat)
      if (allocated(sr%pntfilnam)) deallocate (sr%pntfilnam, stat=istat)
      if (allocated(sr%pntfilnamtab)) deallocate (sr%pntfilnamtab, stat=istat)
      if (allocated(sr%add_out_names)) deallocate (sr%add_out_names, stat=istat)
      if (allocated(sr%extforce_names_gen)) deallocate (sr%extforce_names_gen, stat=istat)
   end subroutine dealloc_swan
!
!
!==============================================================================
   subroutine read_dwaves_mdw(filnam, sr, wavedata)
      implicit none
      !
      character(*) :: filnam
      type(swan_type) :: sr
      type(wave_data_type) :: wavedata
      !
      integer :: indend
      integer :: indstart
      integer :: iuntim
      integer :: istat
      integer :: it01
      real :: tscale
      logical :: ex
      logical :: keywbased
      character(256) :: line
      !
      sr%filnam = filnam
      sr%prname = ''
      sr%prnumb = ''
      sr%title1 = ''
      sr%title2 = ''
      sr%title3 = ''
      sr%comfile = ''
      sr%flowgridfile = ' '
      sr%useflowdata = .false.
      sr%exemode = SWAN_MODE_EXE
      sr%swmor = .false.
      sr%swveg = .false.
      sr%swwlt = .false.
      sr%swuvt = .false.
      sr%swwindt = .false.
      sr%timedependent = .false.
      !
      keywbased = .false.
      call read_keyw_mdw(sr, wavedata, keywbased)
      if (.not. keywbased) then
         ! The old mdw file format is no longer supported
         write (*, *) '*** ERROR: This .mdw file format is no longer supported. Consult the D-Waves user manual for examples of keyword based .mdw files.'
         goto 999
      end if
      indend = index(filnam, '.mdw')
      indstart = max(0, index(filnam, '/', back=.true.), index(filnam, '\', back=.true.))
      sr%casl = filnam(indstart + 1:indend - 1)
      return
999   continue
      write (*, '(a)') '*** ERROR: While reading file ''waves_alone''.'
      call wavestop(1, '*** ERROR: While reading file ''waves_alone''.')
   end subroutine read_dwaves_mdw
!
!
!==============================================================================
   subroutine read_keyw_mdw(sr, wavedata, keywbased)
      use properties
      use read_grids
      use time_module, only: ymd2jul
      use string_module
      use netcdf_utils, only: ncu_format_to_cmode
      use system_utils, only: SCRIPT_EXTENSION
      use wave_mpi, only: engine_comm_world, MPI_COMM_NULL
      implicit none
      !
      type(swan_type) :: sr
      type(wave_data_type) :: wavedata
      logical :: keywbased
      !
      type(tree_data), pointer :: mdw_ptr
      type(tree_data), pointer :: gen_ptr
      type(tree_data), pointer :: out_ptr
      type(tree_data), pointer :: node_ptr
      type(tree_data), pointer :: obst_ptr
      type(tree_data), pointer :: pol_ptr
      type(tree_data), pointer :: bnd_ptr
      type(tree_data), pointer :: tmp_ptr
      integer :: boundnr
      integer :: count
      integer :: def_dirspace
      integer :: def_ndir
      integer :: def_nfreq
      integer :: domainnr
      integer :: i
      integer :: ii
      integer :: io
      integer :: istat
      integer :: j
      integer :: nbound
      integer :: nlocc
      integer :: jj
      integer :: ndomains
      integer :: nobst
      integer :: nobstpnt
      integer :: nsect
      integer :: ntimes
      integer :: obstnr
      integer :: obstpnt
      integer :: refdate
      integer :: sectnr
      integer :: timenr
      integer :: timetable
      integer :: n_outpars
      integer :: par
      integer :: slash_er
      integer :: slash_ok
      integer :: old_input
      integer :: loc_tag
      integer :: ierr
      integer, dimension(4) :: def_ts_hs
      integer, dimension(4) :: def_ts_tp
      integer, dimension(4) :: def_ts_wd
      integer, dimension(4) :: def_ts_ds
      logical :: ex !< flag indicating whether file exists
      logical :: flag
      logical :: success
      real :: def_startdir
      real :: def_enddir
      real :: def_freqmin
      real :: def_freqmax
      real :: tscale
      real, dimension(2) :: xy
      character(10) :: exemode
      character(10) :: versionstring
      character(37) :: obstfil
      character(37) :: tseriesfilename
      character(37) :: polylinefile
      character(80) :: parname
      character(256) :: rec
      character(256) :: errorstring
      character(80), dimension(:), allocatable :: tmp_add_out_names
      character(80), dimension(:), allocatable :: tmp_extforce_names
      character(1), dimension(:), pointer :: data_ptr
      type(swan_bnd), pointer :: bnd
      type(swan_dom), pointer :: dom
      !
      ! Try opening wave file as keyword based file
      !
      nullify (mdw_ptr)
      call tree_create('Delft3D-WAVE input', mdw_ptr)
      istat = 0
      call prop_file('ini', trim(sr%filnam), mdw_ptr, istat)
      if (istat /= 0) then
         select case (istat)
         case (1)
            call wavestop(1, '*** ERROR File: '//trim(sr%filnam)//' not found')
         case (3)
            call wavestop(1, '*** ERROR Premature EOF in file: '//trim(sr%filnam))
         case default
            call wavestop(1, '*** ERROR Read error from file: '//trim(sr%filnam))
         end select
      end if
      !
      ! Check version number of wave input file
      !
      versionstring = ''
      call prop_get(mdw_ptr, 'WaveFileInformation', 'FileVersion', versionstring)
      if (trim(versionstring) < '02.00') return
      keywbased = .true.
      !
      !
      call tree_get_node_by_name(mdw_ptr, 'General', gen_ptr)
      ! Read the Template INPUT keyword before everything else
      sr%inputtemplatefile = ''
      call prop_get(mdw_ptr, 'General', 'INPUTTemplateFile', sr%inputtemplatefile)
      if (sr%inputtemplatefile /= '') then
         inquire (file=trim(sr%inputtemplatefile), exist=ex)
         if (.not. ex) then
            write (*, *) 'SWAN_INPUT: specified INPUTTemplateFile "', trim(sr%inputtemplatefile), '" does not exist.'
            call wavestop(1, '*** ERROR File: '//trim(sr%inputtemplatefile)//' not found')
         end if
      end if

      ! gen_ptr is used later on
      !
      !
      ! From here on we know that we read the input data from a keyword based
      ! mdw file.
      !
      call prop_get(mdw_ptr, 'General', 'Projectname', sr%prname)
      call prop_get(mdw_ptr, 'General', 'Projectnr', sr%prnumb)
      call prop_get(mdw_ptr, 'General', 'Description1', sr%title1)
      call prop_get(mdw_ptr, 'General', 'Description2', sr%title2)
      call prop_get(mdw_ptr, 'General', 'Description3', sr%title3)
      call prop_get(mdw_ptr, 'General', 'OnlyInputVerify', flag)
      sr%compmode = .not. flag
      !
      exemode = 'exe'
      sr%scriptname = ' '
      call prop_get(mdw_ptr, 'General', 'SwanMode', exemode)
      call str_lower(exemode)
      select case (exemode)
      case ('exe')
         sr%exemode = SWAN_MODE_EXE
      case ('lib')
         sr%exemode = SWAN_MODE_LIB
         if (engine_comm_world == MPI_COMM_NULL) then
            write (*, *) 'SWAN_INPUT: SwanMode = lib only allowed when D-Waves is run using MPI.'
            call handle_errors_mdw(sr)
         end if
         call prop_get(mdw_ptr, 'General', 'ScriptName', sr%scriptname)
         if (sr%scriptname /= ' ') then
            sr%scriptname = trim(sr%scriptname)//SCRIPT_EXTENSION
            inquire (file=trim(sr%scriptname), exist=ex)
            if (.not. ex) then
               write (*, *) 'SWAN_INPUT: specified ScriptName "', trim(sr%scriptname), '" does not exist.'
               call handle_errors_mdw(sr)
            end if
         end if
      case default
         write (*, *) 'SWAN_INPUT: invalid SWAN execution mode. Expected SwanMode = "exe" or "lib"'
         call handle_errors_mdw(sr)
      end select
      !
      sr%deltc = -999.0
      sr%nonstat_interval = -999.0
      parname = ''

      if (swan_run%inputtemplatefile /= '') then
         ! Read ModSim from SWAN INPUT file when using a template
         open (newunit=old_input, file=swan_run%inputtemplatefile, form='formatted', status='old', iostat=ierr)
         if (ierr /= 0) then
            write (*, '(2a)') '*** ERROR: Unable to find file ', trim(swan_run%inputtemplatefile)
            close (old_input)
            call wavestop(1, 'Unable to find file '//trim(swan_run%inputtemplatefile))
         end if

         read (old_input, '(a)', iostat=ierr) rec
         if (ierr /= 0) then
            write (*, '(2a)') '*** ERROR: Unable to read file ', trim(swan_run%inputtemplatefile)
            close (old_input)
            call wavestop(1, 'Unable to read file '//trim(swan_run%inputtemplatefile))
         end if
         do while (ierr == 0)
            loc_tag = index(rec, 'MODE ')
            if (loc_tag /= 0) then
               if (index(rec, ' STAT') /= 0) then
                  ! (quasi-)stationary mode: NB we cannot distinguish quasi-stat from stat
                  parname = 'stationary'
               elseif (index(rec, 'NONST') /= 0) then
                  ! non-stationary mode
                  parname = 'non-stationary'
               end if
               exit
            end if
            read (old_input, '(a)', iostat=ierr) rec
         end do
      else
         call prop_get(mdw_ptr, 'General', 'SimMode', parname)
      end if

      select case (parname)
      case ('stationary')
         !
         ! Use modsim = 2 also for stationary input
         !
         sr%modsim = 2
      case ('quasi-stationary')
         sr%modsim = 2
      case ('non-stationary')
         sr%modsim = 3
         call prop_get(mdw_ptr, 'General', 'TimeStep', sr%deltc)
         if (sr%deltc < 0.0) then
            write (*, *) '*** ERROR: Unable to read non-stationary parameter "TimeStep"'
            call handle_errors_mdw(sr)
         end if
         call prop_get(mdw_ptr, 'General', 'TimeInterval', sr%nonstat_interval)
         if (sr%nonstat_interval < 0.0) then
            write (*, *) '*** ERROR: Unable to read non-stationary parameter "TimeInterval"'
            call handle_errors_mdw(sr)
         end if
      case default
         write (*, *) 'SWAN_INPUT: missing or invalid simulation mode'
         call handle_errors_mdw(sr)
      end select
      !
      parname = ''
      call prop_get(mdw_ptr, 'General', 'FlowFile', parname)
      if (parname /= ' ') then
         call setmode(wavedata, flow_online)
         parname = ''
         call prop_get(mdw_ptr, 'General', 'FlowMudFile', parname)
         if (parname /= ' ') then
            call setmode(wavedata, flow_mud_online)
         end if
      else
         call prop_get(mdw_ptr, 'General', 'ComFile', sr%comfile)
      end if
      select case (wavedata%mode)
      case (stand_alone)
         write (*, '(a)') '*** MESSAGE: Delft3D-WAVE runs stand alone'
      case (flow_online)
         write (*, '(a)') '*** MESSAGE: Delft3D-WAVE runs online with Delft3D-FLOW'
      case (flow_mud_online)
         write (*, '(a)') '*** MESSAGE: Delft3D-WAVE runs online with Delft3D-FLOW, including MUD'
      end select
      !
      parname = ''
      call prop_get(mdw_ptr, 'General', 'DirConvention', parname)
      call str_lower(parname, len(parname))
      select case (parname)
      case ('nautical')
         sr%nautconv = .true.
      case ('cartesian')
         sr%nautconv = .false.
      case default
         write (*, *) 'SWAN_INPUT: missing or invalid direction convention'
         call handle_errors_mdw(sr)
      end select
      obstfil = ''
      call prop_get(mdw_ptr, 'General', 'ObstacleFile', obstfil)
      !
      ! Determine reference date. Date string converted from YYYY-MM-DD
      ! to YYYYMMDD.
      !
      parname = ''
      call prop_get(mdw_ptr, 'General', 'ReferenceDate', parname)
      parname(5:6) = parname(6:7)
      parname(7:8) = parname(9:10)
      parname(9:) = ' '
      read (parname, *, iostat=istat) refdate
      if (istat /= 0) then
         write (*, *) 'SWAN_INPUT: missing or invalid reference date'
         call handle_errors_mdw(sr)
      end if
      call setrefdate(wavedata%time, refdate)
      sr%refjulday = ymd2jul(refdate)
      !
      sr%tzone = 0.0
      call prop_get(mdw_ptr, 'General', 'TZone', sr%tzone)
      !
      tscale = 60.0
      call prop_get(mdw_ptr, 'General', 'TScale', tscale)
      call settscale(wavedata%time, tscale)
      !
      tseriesfilename = ''
      call prop_get(mdw_ptr, 'General', 'TSeriesFile', tseriesfilename)
      if (tseriesfilename /= ' ') then
         sr%timedependent = .true.
         call readtable(sr%tseriesfile, tseriesfilename, sr%refjulday, errorstring)
         if (errorstring /= ' ') then
            write (*, '(A)') trim(errorstring)
            call handle_errors_mdw(sr)
         end if
      end if
      sr%flowLinkConnectivity = .false.
      call prop_get(mdw_ptr, 'General', 'flowLinkConnectivity', sr%flowLinkConnectivity)
      !
      ! Write format for SWAN input
      sr%ndec = 8
      call prop_get(mdw_ptr, 'General', 'NDec', sr%ndec)
      !
      ! Time points
      !
      timetable = -999
      if (sr%timedependent) then
         call prop_get(mdw_ptr, 'General', 'TimePntBlock', timetable)
      end if
      if (timetable > 0) then
         !
         ! time points from TSeriesFile
         !
         ntimes = gettablentimes(sr%tseriesfile, timetable, errorstring)
         if (errorstring /= ' ') then
            write (*, '(A)') trim(errorstring)
            call handle_errors_mdw(sr)
         end if
      else
         !
         ! Count number of time points
         !
         ntimes = 0
         do i = 1, size(mdw_ptr%child_nodes)
            tmp_ptr => mdw_ptr%child_nodes(i)%node_ptr
            parname = tree_get_name(tmp_ptr)
            if (parname == 'timepoint') ntimes = ntimes + 1
         end do
         !
      end if
      ntimes = max(ntimes, 1)
      sr%nttide = ntimes
      !
      istat = 0
      allocate (sr%timwav(ntimes), stat=istat)
      if (istat == 0) allocate (sr%zeta(ntimes), stat=istat)
      if (istat == 0) allocate (sr%ux0(ntimes), stat=istat)
      if (istat == 0) allocate (sr%uy0(ntimes), stat=istat)
      if (istat == 0) allocate (sr%wdir(ntimes), stat=istat)
      if (istat == 0) allocate (sr%wvel(ntimes), stat=istat)
      !
      if (istat /= 0) then
         write (*, *) 'SWAN_INPUT: memory alloc error (ntimes)'
         call handle_errors_mdw(sr)
      end if
      !
      sr%timwav = -999.0
      sr%zeta = 0.0
      sr%ux0 = 0.0
      sr%uy0 = 0.0
      sr%wvel = 0.0
      sr%wdir = 0.0
      sr%swuvi = .true.
      !
      ! Get default values for time-varying quantities
      !
      call prop_get(mdw_ptr, 'General', 'WaterLevel', sr%zeta(1))
      sr%zeta = sr%zeta(1)
      !
      call prop_get(mdw_ptr, 'General', 'XVeloc', sr%ux0(1))
      sr%ux0 = sr%ux0(1)
      !
      call prop_get(mdw_ptr, 'General', 'YVeloc', sr%uy0(1))
      sr%uy0 = sr%uy0(1)
      !
      call prop_get(mdw_ptr, 'General', 'WindSpeed', sr%wvel(1))
      sr%wvel = sr%wvel(1)
      !
      call prop_get(mdw_ptr, 'General', 'WindDir', sr%wdir(1))
      sr%wdir = sr%wdir(1)
      if (timetable > 0) then
         !
         ! Times obtained from table module are in hours
         !
         call gettabletimes(sr%tseriesfile, timetable, sr%timwav, sr%refjulday, &
                          & errorstring)
         sr%timwav = sr%timwav * 60.0
      else
         timenr = 0
         i = 1
         do j = 1, size(mdw_ptr%child_nodes)
            tmp_ptr => mdw_ptr%child_nodes(j)%node_ptr
            parname = tree_get_name(tmp_ptr)
            select case (parname)
            case ('timepoint')
               timenr = timenr + 1
               i = timenr
               call prop_get(tmp_ptr, '*', 'Time', sr%timwav(timenr))
               call prop_get(tmp_ptr, '*', 'WaterLevel', sr%zeta(i))
               if (timenr == 0) sr%zeta = sr%zeta(1)
               call prop_get(tmp_ptr, '*', 'XVeloc', sr%ux0(i))
               if (timenr == 0) sr%ux0 = sr%ux0(1)
               call prop_get(tmp_ptr, '*', 'YVeloc', sr%uy0(i))
               if (timenr == 0) sr%uy0 = sr%uy0(1)
               call prop_get(tmp_ptr, '*', 'WindSpeed', sr%wvel(i))
               if (timenr == 0) sr%wvel = sr%wvel(1)
               call prop_get(tmp_ptr, '*', 'WindDir', sr%wdir(i))
               if (timenr == 0) sr%wdir = sr%wdir(1)
            case default
               !
               ! nothing
               !
            end select
         end do
      end if
      !
      ! The following if statement is added to compare with old version
      !
      if (sr%ux0(1) == 0.0 .and. sr%uy0(1) == 0.0) then
         sr%swuvi = .false.
      end if
      !
      !
      ! Optionally find these general quantities in the tables
      !
      if (sr%timedependent) then
         call gettable(sr%tseriesfile, 'General', 'WaterLevel', sr%ts_wl, &
                     & 0, errorstring)
         if (sr%ts_wl(3) > 1) then
            write (*, *) 'SWAN_INPUT: too many WaterLevel entries in TSeriesFile'
            call handle_errors_mdw(sr)
         end if
         !
         call gettable(sr%tseriesfile, 'General', 'XVeloc', sr%ts_xv, &
                     & 0, errorstring)
         if (sr%ts_xv(3) > 1) then
            write (*, *) 'SWAN_INPUT: too many XVeloc entries in TSeriesFile'
            call handle_errors_mdw(sr)
         end if
         !
         call gettable(sr%tseriesfile, 'General', 'YVeloc', sr%ts_yv, &
                     & 0, errorstring)
         if (sr%ts_yv(3) > 1) then
            write (*, *) 'SWAN_INPUT: too many YVeloc entries in TSeriesFile'
            call handle_errors_mdw(sr)
         end if
         !
         call gettable(sr%tseriesfile, 'General', 'WindSpeed', sr%ts_ws, &
                     & 0, errorstring)
         if (sr%ts_ws(3) > 1) then
            write (*, *) 'SWAN_INPUT: too many WindSpeed entries in TSeriesFile'
            call handle_errors_mdw(sr)
         end if
         !
         call gettable(sr%tseriesfile, 'General', 'WindDir', sr%ts_wd, &
                     & 0, errorstring)
         if (sr%ts_wd(3) > 1) then
            write (*, *) 'SWAN_INPUT: too many WindDir entries in TSeriesFile'
            call handle_errors_mdw(sr)
         end if
      end if
      !
      ! Default settings for domains
      !
      parname = ''
      call prop_get(mdw_ptr, 'General', 'DirSpace', parname)
      call str_lower(parname, len(parname))
      def_dirspace = -999
      select case (parname)
      case ('circle')
         def_dirspace = 1
      case ('sector')
         def_dirspace = 2
      case default
         if (parname /= '') then
            write (*, *) 'SWAN_INPUT: unknown General/DirSpace: ', parname
            call handle_errors_mdw(sr)
         end if
      end select
      def_ndir = -999
      def_startdir = -999.0
      def_enddir = -999.0
      def_nfreq = -999
      def_freqmin = -999.0
      def_freqmax = -999.0
      sr%veg_drag = -999.0
      call prop_get(mdw_ptr, 'General', 'NDir', def_ndir)
      call prop_get(mdw_ptr, 'General', 'StartDir', def_startdir)
      call prop_get(mdw_ptr, 'General', 'EndDir', def_enddir)
      call prop_get(mdw_ptr, 'General', 'NFreq', def_nfreq)
      call prop_get(mdw_ptr, 'General', 'FreqMin', def_freqmin)
      call prop_get(mdw_ptr, 'General', 'FreqMax', def_freqmax)
      !
      ! Count number of external forcing files listed in group General
      !
      call count_occurrences(mdw_ptr, 'General', 'extforce', count)
      sr%n_extforces_gen = count
      call count_occurrences(mdw_ptr, 'General', 'meteofile', count)
      sr%n_extforces_gen = sr%n_extforces_gen + count
      sr%n_meteofiles_gen = count
      !
      if (sr%n_extforces_gen > 0) then
         !
         ! Allocate temporary array for names of external forcing files
         !
         allocate (tmp_extforce_names(sr%n_extforces_gen), stat=istat)
         tmp_extforce_names = ''
         !
         ! The input can be read
         !
         par = 0
         call tree_get_node_by_name(mdw_ptr, 'General', tmp_ptr)
         do j = 1, size(tmp_ptr%child_nodes)
            node_ptr => tmp_ptr%child_nodes(j)%node_ptr
            parname = tree_get_name(node_ptr)
            if (parname == 'extforce' .or. parname == 'meteofile') then
               par = par + 1
               !
               ! Read value for keyword
               !
               call tree_get_data_string(node_ptr, parname, success)
               !
               ! Check double occurrences
               !
               do i = 1, par
                  if (tmp_extforce_names(i) == parname) then
                     write (*, '(a,a,a)') 'SWAN input: Group General: External forcing ', trim(parname), ' has already been read'
                     par = par - 1
                     sr%n_extforces_gen = sr%n_extforces_gen - 1
                     if (parname == 'meteofile') sr%n_meteofiles_gen = sr%n_meteofiles_gen - 1
                     exit
                  end if
               end do
               !
               if (tmp_extforce_names(par) == '') then
                  !
                  ! Index par points to empty names, so store the file name in the array
                  !
                  tmp_extforce_names(par) = trim(parname)
               end if
               if (par == sr%n_extforces_gen) then
                  !
                  ! All external forces read, stop processing keywords
                  !
                  exit
               end if
            end if
         end do
         !
         ! Allocate array for names of external forcings in group General
         !
         allocate (sr%extforce_names_gen(sr%n_extforces_gen), stat=istat)
         sr%extforce_names_gen(1:sr%n_extforces_gen) = tmp_extforce_names(1:sr%n_extforces_gen)
         deallocate (tmp_extforce_names)
         !
         if (sr%n_meteofiles_gen > 0) sr%swwindt = .true.
         !
      end if
      !
      ! Minimum number of surrounding source-points
      !
      sr%msurpnts = 3
      call prop_get(mdw_ptr, 'General', 'MinSurroundPoints', sr%msurpnts)
      if (sr%msurpnts /= 3) then
         write (*, *) "Minimum number of surrounding valid source-points for a target-point to be covered: ", sr%msurpnts
      end if
      !
      ! Constants
      !
      sr%grav = 9.81
      sr%rho = 1025.0
      sr%northdir = 90.0
      sr%depmin = 0.05
      sr%inrhog = 1
      sr%wlevelcorr = 0.0
      sr%maxerr = 2
      call prop_get(mdw_ptr, 'Constants', 'Gravity', sr%grav)
      call prop_get(mdw_ptr, 'Constants', 'WaterDensity', sr%rho)
      call prop_get(mdw_ptr, 'Constants', 'NorthDir', sr%northdir)
      call prop_get(mdw_ptr, 'Constants', 'MinimumDepth', sr%depmin)
      call prop_get(mdw_ptr, 'Constants', 'WaterLevelCorrection', sr%wlevelcorr)
      call prop_get(mdw_ptr, 'Constants', 'MaxErrorLevel', sr%maxerr)
      !
      ! Processes
      !
      sr%genmode = -999
      sr%setup = .false.
      sr%breaking = .true.
      sr%cfbr1 = 1.0
      sr%cfbr2 = 0.73
      sr%triads = .false.
      sr%cftriad1 = 0.8
      sr%cftriad2 = 2.2
      sr%frictype = 1
      sr%frcof = 0.067
      sr%diffr_coeff = 0.2
      sr%diffr_smsteps = 5
      sr%windgrowth = .true.
      sr%whitecap = 1
      sr%quadruplets = .false.
      sr%refraction = .true.
      sr%fshift = .true.
      sr%alfawind = 1.0
      !
      call prop_get(mdw_ptr, 'Processes', 'GenModePhys', sr%genmode)
      if (sr%genmode < 0 .or. sr%genmode > 3) then
         write (*, *) 'SWAN_INPUT: missing or invalid generation mode'
         call handle_errors_mdw(sr)
      end if
      !
      call prop_get(mdw_ptr, 'Processes', 'WaveSetup', sr%setup)
      call prop_get(mdw_ptr, 'Processes', 'Breaking', sr%breaking)
      if (sr%breaking) then
         call prop_get(mdw_ptr, 'Processes', 'BreakAlpha', sr%cfbr1)
         call prop_get(mdw_ptr, 'Processes', 'BreakGamma', sr%cfbr2)
      end if
      call prop_get(mdw_ptr, 'Processes', 'Triads', sr%triads)
      if (sr%triads) then
         call prop_get(mdw_ptr, 'Processes', 'TriadsAlpha', sr%cftriad1)
         call prop_get(mdw_ptr, 'Processes', 'TriadsBeta', sr%cftriad2)
      end if
      !
      parname = ''
      call prop_get(mdw_ptr, 'Processes', 'BedFriction', parname)
      call str_lower(parname, len(parname))
      select case (parname)
      case ('none', ' ')
         sr%frictype = 0
         sr%frcof = 0.0
      case ('jonswap')
         sr%frictype = 1
         sr%frcof = 0.067
      case ('collins')
         sr%frictype = 2
         sr%frcof = 0.015
      case ('madsen et al.')
         sr%frictype = 3
         sr%frcof = 0.05
      case default
         write (*, *) 'SWAN_INPUT: invalid bed friction type'
         call handle_errors_mdw(sr)
      end select
      if (sr%frictype > 0) then
         call prop_get(mdw_ptr, 'Processes', 'BedFricCoef', sr%frcof)
      end if
      !
      flag = .true.
      sr%diffraction = 1
      call prop_get(mdw_ptr, 'Processes', 'Diffraction', flag)
      if (.not. flag) sr%diffraction = 0
      if (sr%diffraction == 1) then
         call prop_get(mdw_ptr, 'Processes', 'DiffracCoef', sr%diffr_coeff)
         call prop_get(mdw_ptr, 'Processes', 'DiffracSteps', sr%diffr_smsteps)
         !
         flag = .true.
         sr%diffr_adapt_propag = 1
         call prop_get(mdw_ptr, 'Processes', 'DiffracProp', flag)
         if (.not. flag) sr%diffr_adapt_propag = 0
      end if
      !
      call prop_get(mdw_ptr, 'Processes', 'WindGrowth', sr%windgrowth)
      call prop_get(mdw_ptr, 'Processes', 'AlfaWind', sr%alfawind)
      if (sr%alfawind < 1d-6 .and. sr%alfawind > -1d-6) then
         write (*, '(a)') 'SWAN_INPUT: AlfaWind is not allowed to be equal to 0.0.'
         call handle_errors_mdw(sr)
      end if
      parname = ''
      call prop_get(mdw_ptr, 'Processes', 'WhiteCapping', parname)
      call str_lower(parname, len(parname))
      select case (parname)
      case ('off')
         sr%whitecap = WC_OFF
      case ('komen', ' ')
         sr%whitecap = WC_KOMEN
      case ('westhuysen')
         sr%whitecap = WC_WESTHUYSEN
         if (sr%genmode /= 3) then
            write (*, '(2a,i0)') 'SWAN_INPUT: WhiteCapping=Westhuysen can not be', &
                 & ' combined with formulations of generation ', sr%genmode
            call handle_errors_mdw(sr)
         end if
      case default
         write (*, *) 'SWAN_INPUT: [Processes] WhiteCapping: invalid input:', trim(parname)
         call handle_errors_mdw(sr)
      end select
      call prop_get(mdw_ptr, 'Processes', 'Quadruplets', sr%quadruplets)
      call prop_get(mdw_ptr, 'Processes', 'Refraction', sr%refraction)
      call prop_get(mdw_ptr, 'Processes', 'FreqShift', sr%fshift)
      !
      parname = ''
      call prop_get(mdw_ptr, 'Processes', 'WaveForces', parname)
      call str_lower(parname, len(parname))
      select case (parname)
      case ('radiation stresses <2013')
         sr%swdis = 1
      case ('radiation stresses', ' ')
         write (*, *) 'SWAN_INPUT: [Processes] WaveForces is set to "dissipation 3d"'
         write (*, *) 'To switch on radiation stresses: "WaveForces = radiation stresses <2013"'
         sr%swdis = 3
      case ('dissipation')
         sr%swdis = 2
      case ('dissipation 3d')
         sr%swdis = 3
      case default
         write (*, *) 'SWAN_INPUT: invalid method to compute wave forces'
         call handle_errors_mdw(sr)
      end select
      !
      parname = ''
      call prop_get(mdw_ptr, 'Processes', 'IceDamp', parname)
      call str_lower(parname, len(parname))
      select case (parname)
      case ('none', ' ')
         sr%icedamp = 0
      case ('clip')
         sr%icedamp = 1
      case ('swan')
         sr%icedamp = 2
         ! Default settings of Meylan et al (2014)
         sr%icecoeff = 0.0
         sr%icecoeff(3) = 1.06e-3
         sr%icecoeff(5) = 2.3e-2
         call prop_get(mdw_ptr, 'Processes', 'IceCoef', sr%icecoeff, 7)
         ! Icewind
         sr%icewind = 0.0
         call prop_get(mdw_ptr, 'Processes', 'IceWind', sr%icewind)
      case default
         write (*, *) 'SWAN_INPUT: invalid method for wave damping due to ice'
         call handle_errors_mdw(sr)
      end select
      !
      ! Numerics
      !
      sr%num_scheme = 1
      sr%cdd = 0.5
      sr%css = 0.5
      sr%drel = 0.02
      sr%dh_abs = 0.02
      sr%dt_abs = 0.02
      sr%percwet = 98.0
      sr%itermx = 15
      sr%gamma0 = 3.3
      sr%alfa = 0.0
      !
      parname = ''
      call prop_get(mdw_ptr, 'Numerics', 'Scheme', parname)
      call str_lower(parname, len(parname))
      select case (parname)
      case ('default')
         sr%num_scheme = 1
      case ('bsbt')
         sr%num_scheme = 2
         write (*, *) 'SWAN_INPUT: Numerics/Scheme = BSBT'
      case default
         if (parname /= '') then
            write (*, *) 'SWAN_INPUT: unknown Numerics/Scheme: ', parname
            goto 999
         end if
      end select
      call prop_get(mdw_ptr, 'Numerics', 'DirSpaceCDD', sr%cdd)
      call prop_get(mdw_ptr, 'Numerics', 'FreqSpaceCSS', sr%css)
      call prop_get(mdw_ptr, 'Numerics', 'RChHsTm01', sr%drel)
      call prop_get(mdw_ptr, 'Numerics', 'RChMeanHs', sr%dh_abs)
      call prop_get(mdw_ptr, 'Numerics', 'RChMeanTm01', sr%dt_abs)
      call prop_get(mdw_ptr, 'Numerics', 'PercWet', sr%percwet)
      call prop_get(mdw_ptr, 'Numerics', 'MaxIter', sr%itermx)
      call prop_get(mdw_ptr, 'Numerics', 'AlfaUnderRelax', sr%alfa)
      !
      ! General output options
      !
      sr%itest = 0
      sr%itrace = 0
      sr%hotfile = .false.
      sr%wavm_write_interval = 0.0
      sr%swwav = .false.
      sr%deltcom = 0.0
      sr%flowgridfile = ' '
      sr%append_com = .false.
      sr%output_points = .false.
      sr%output_pnt_file = .false.
      sr%pntfil = ' '
      sr%curvefil = ' '
      sr%swflux = .true.
      sr%swmapwritenetcdf = .true.
      sr%netcdf_sp = .false.
      sr%keepinput = .false.
      par = 0
      !
      ! Standard output options
      !
      call prop_get(mdw_ptr, 'Output', 'TestOutputLevel', sr%itest)
      call prop_get(mdw_ptr, 'Output', 'TraceCalls', flag)
      if (flag) sr%itrace = 1
      call prop_get(mdw_ptr, 'Output', 'UseHotFile', sr%hotfile)
      call prop_get(mdw_ptr, 'Output', 'MapWriteInterval', sr%wavm_write_interval)
      call prop_get(mdw_ptr, 'Output', 'WriteCOM', sr%swwav)
      call prop_get(mdw_ptr, 'Output', 'MassFluxToCOM', sr%swflux)
      call prop_get(mdw_ptr, 'Output', 'COMWriteInterval', sr%deltcom)
      call prop_get(mdw_ptr, 'Output', 'FlowGridForCom', sr%flowgridfile)
      if (sr%flowgridfile /= ' ') then
         write (*, '(a)') "ERROR: No longer supported: stand alone WAVE computation using FLOW data in a com-file via keyword 'FlowGridForCom'"
         call handle_errors_mdw(sr)
      end if
      call prop_get(mdw_ptr, 'Output', 'COMFile', sr%flowgridfile)
      call prop_get(mdw_ptr, 'Output', 'AppendCOM', sr%append_com)
      call prop_get(mdw_ptr, 'Output', 'MapWriteNetCDF', sr%swmapwritenetcdf)
      call prop_get(mdw_ptr, 'Output', 'NetCDFSinglePrecision', sr%netcdf_sp)
      call prop_get(mdw_ptr, 'Output', 'KeepINPUT', sr%keepinput)
      call prop_get(mdw_ptr, 'Output', 'ncFormat', par)
      call set_ncmode(wavedata%output, ncu_format_to_cmode(par))
      !
      sr%output_ice = 0
      if (sr%icedamp > 0) then
         flag = .false.
         call prop_get(mdw_ptr, 'Output', 'IceOut', flag)
         if (flag) sr%output_ice = sr%icedamp
      end if
      !
      ! Check that the comfile is not a map file (not allowed. Was allowed in preliminary versions)
      !
      if (sr%flowgridfile /= ' ') then
         i = len_trim(sr%flowgridfile)
         if (sr%flowgridfile(i - 5:i) /= "com.nc") then
            write (*, '(3a)') "ERROR: The name of the COMFile (", trim(sr%flowgridfile), ") must end on 'com.nc'"
            call handle_errors_mdw(sr)
         end if
      end if
      !
      ! determine the number of location files
      !
      nlocc = 0
      call tree_get_node_by_name(mdw_ptr, 'Output', out_ptr)
      do jj = 1, size(out_ptr%child_nodes)
         tmp_ptr => out_ptr%child_nodes(jj)%node_ptr
         parname = tree_get_name(tmp_ptr)
         if (parname == 'locationfile') then
            nlocc = nlocc + 1
         end if
      end do
      if (nlocc > 0) then
         sr%output_points = .true.
         sr%output_pnt_file = .true.
         allocate (sr%pntfilnam(nlocc), stat=istat)
         allocate (sr%pntfilnamtab(nlocc), stat=istat)
         if (istat /= 0) then
            write (*, *) 'SWAN_INPUT: memory alloc error (pntfilnam)'
            call handle_errors_mdw(sr)
         end if
         sr%pntfilnam = ' '
         sr%pntfilnamtab = ' '
      end if
      !
      ! read the location files
      !
      nlocc = 0
      call tree_get_node_by_name(mdw_ptr, 'Output', out_ptr)
      do jj = 1, size(out_ptr%child_nodes)
         tmp_ptr => out_ptr%child_nodes(jj)%node_ptr
         parname = tree_get_name(tmp_ptr)
         if (parname == 'locationfile') then
            nlocc = nlocc + 1
            call prop_get(tmp_ptr, '*', 'locationfile', sr%pntfilnam(nlocc))
         end if
      end do
      sr%nloc = nlocc
      sr%output_table = .false.
      sr%output_spec1d = .false.
      sr%output_spec2d = .false.
      if (sr%output_pnt_file) then
         call prop_get(mdw_ptr, 'Output', 'WriteTable', sr%output_table)
         call prop_get(mdw_ptr, 'Output', 'WriteSpec1D', sr%output_spec1d)
         call prop_get(mdw_ptr, 'Output', 'WriteSpec2D', sr%output_spec2d)
      end if
      call prop_get(mdw_ptr, 'Output', 'CurveFile', sr%curvefil)
      if (sr%curvefil /= ' ') then
         !
         ! unknown number of output curves defined in a Tekal file
         ! use ncurv = -1 to flag this
         !
         sr%ncurv = -1
      end if
      !
      ! Additional keywords? Count the number of occurrences
      !
      n_outpars = 0
      call count_occurrences(mdw_ptr, 'output', 'additionaloutput', n_outpars)
      if (n_outpars > 0) then
         !
         ! Allocate temporary array for additional output parameters
         !
         allocate (tmp_add_out_names(n_outpars), stat=istat)
         !
         ! Initialize array
         !
         tmp_add_out_names = ' '
         !
         ! The input can be read
         !
         par = 0
         !
         call tree_get_node_by_name(mdw_ptr, 'output', out_ptr)
         do j = 1, size(out_ptr%child_nodes)
            !
            ! Does out_ptr contain one or more children with name AdditionalOutput?
            !
            tmp_ptr => out_ptr%child_nodes(j)%node_ptr
            parname = tree_get_name(tmp_ptr)
            if (parname == 'additionaloutput') then
               par = par + 1
               !
               ! Read the additional output parameter
               !
               call tree_get_data_string(tmp_ptr, parname, success)
               do i = 1, par
                  if (tmp_add_out_names(i) == parname) then
                     write (*, '(3a)') 'SWAN input: Additional output parameter ', trim(parname), ' has already been read'
                     par = par - 1
                     n_outpars = n_outpars - 1
                     exit
                  end if
               end do
               !
               if (tmp_add_out_names(par) == ' ') then
                  tmp_add_out_names(par) = trim(parname)
               end if
               if (par == n_outpars) then
                  exit
               end if
            end if
         end do
         !
         ! Allocate array for additional output parameters
         !
         allocate (sr%add_out_names(n_outpars), stat=istat)
         !
         ! Fill the array with additional output names
         !
         sr%add_out_names(1:n_outpars) = tmp_add_out_names(1:n_outpars)
         !
         deallocate (tmp_add_out_names)
         !
      end if
      !
      !  Interval to keep the hotfile
      !
      sr%int2keephotfile = 0.0
      call prop_get(mdw_ptr, 'Output', 'Int2KeepHotfile', sr%int2keephotfile)
      !
      !  Determine the number of domains
      !
      ndomains = 0
      do i = 1, size(mdw_ptr%child_nodes)
         tmp_ptr => mdw_ptr%child_nodes(i)%node_ptr
         parname = tree_get_name(tmp_ptr)
         if (parname == 'domain') ndomains = ndomains + 1
      end do
      if (ndomains == 0) then
         write (*, *) 'SWAN_INPUT: no domains found!'
         call handle_errors_mdw(sr)
      end if
      sr%nnest = ndomains
      !
      istat = 0
      allocate (sr%dom(ndomains), stat=istat)
      !
      if (istat /= 0) then
         write (*, *) 'SWAN_INPUT: memory alloc error (ndomains)'
         call handle_errors_mdw(sr)
      end if
      !
      sr%dom(1)%qextnd = 0
      sr%dom(1)%flowVelocityType = FVT_DEPTH_AVERAGED
      !
      ! Put general FLOW mapping flags in dom(1)
      !
      if (wavedata%mode /= stand_alone) then
         call prop_get(mdw_ptr, 'General', 'FlowBedLevel', sr%dom(1)%qextnd(q_bath))
         call prop_get(mdw_ptr, 'General', 'FlowVegetation', sr%dom(1)%qextnd(q_veg))
         if (sr%dom(1)%qextnd(q_veg) == 2) then
            write (*, *) 'SWAN_INPUT: FlowVegetation=2 is found while extrapolation to the outside of domain is not supported yet.'
            call handle_errors_mdw(sr)
         end if
         call prop_get(mdw_ptr, 'General', 'FlowWaterLevel', sr%dom(1)%qextnd(q_wl))
         call prop_get(mdw_ptr, 'General', 'FlowVelocity', sr%dom(1)%qextnd(q_cur))
         call prop_get(mdw_ptr, 'General', 'FlowWind', sr%dom(1)%qextnd(q_wind))
         parname = ''
         call prop_get(mdw_ptr, 'General', 'FlowVelocityType', parname)
         call str_lower(parname, len(parname))
         select case (parname)
         case ('depth-averaged')
            sr%dom(1)%flowVelocityType = FVT_DEPTH_AVERAGED
         case ('surface-layer')
            sr%dom(1)%flowVelocityType = FVT_SURFACE_LAYER
         case ('wave-dependent')
            sr%dom(1)%flowVelocityType = FVT_WAVE_DEPENDENT
         case (' ')
            !
            ! Default value used
            !
         case default
            write (*, *) 'SWAN_INPUT: invalid option for [General], FlowVelocityType'
            call handle_errors_mdw(sr)
         end select
      end if
      do i = 1, ndomains
         dom => sr%dom(i)
         dom%curvibot = -999
         dom%dirspace = def_dirspace
         dom%ndir = def_ndir
         dom%nfreq = def_nfreq
         dom%nestnr = -999
         dom%cgnum = .true.
         dom%botfil = ''
         dom%curlif = ''
         dom%depfil = ''
         dom%nesfil = ''
         dom%vegfil = ''
         dom%nesnam = '--DUMMY--' !!dummy
         dom%mxb = -999
         dom%myb = -999
         dom%mxc = -999
         dom%myc = -999
         dom%freqmin = def_freqmin
         dom%freqmax = def_freqmax
         dom%startdir = def_startdir
         dom%enddir = def_enddir
         dom%veg_height = -999.0
         dom%veg_diamtr = -999.0
         dom%veg_nstems = 1
         dom%veg_drag = 1
         dom%qextnd(q_bath) = sr%dom(1)%qextnd(q_bath)
         dom%qextnd(q_veg) = sr%dom(1)%qextnd(q_veg)
         dom%qextnd(q_wl) = sr%dom(1)%qextnd(q_wl)
         dom%qextnd(q_cur) = sr%dom(1)%qextnd(q_cur)
         dom%qextnd(q_wind) = sr%dom(1)%qextnd(q_wind)
         dom%flowVelocityType = sr%dom(1)%flowVelocityType
      end do
      !
      domainnr = 0
      do i = 1, size(mdw_ptr%child_nodes)
         tmp_ptr => mdw_ptr%child_nodes(i)%node_ptr
         parname = tree_get_name(tmp_ptr)
         if (parname /= 'domain') cycle
         domainnr = domainnr + 1
         dom => sr%dom(domainnr)
         !
         ! Read computational grid
         !
         call prop_get(tmp_ptr, '*', 'Grid', dom%curlif)
         call readgriddims(dom%curlif, dom%mxc, dom%myc)
         if (dom%curlif == '') then
            write (*, *) 'SWAN_INPUT: grid not found for domain', domainnr
            call handle_errors_mdw(sr)
         end if
         !
         ! poles? No, fences!
         !
         dom%mxc = dom%mxc - 1
         dom%myc = dom%myc - 1
         !
         ! Read bathymetry
         !
         dom%depfil = ''
         call prop_get(tmp_ptr, '*', 'BedLevelGrid', dom%depfil)
         if (dom%depfil /= '') then
            call readgriddims(dom%depfil, dom%mxb, dom%myb)
            !
            ! poles? No, fences!
            !
            dom%mxb = dom%mxb - 1
            dom%myb = dom%myb - 1
            dom%curvibot = 0
         else
            dom%depfil = dom%curlif
            dom%mxb = dom%mxc
            dom%myb = dom%myc
            dom%curvibot = 1
         end if
         call prop_get(tmp_ptr, '*', 'BedLevel', dom%botfil)
         if (dom%botfil == '') then
            write (*, *) 'SWAN_INPUT: bathymetry not found for domain', domainnr
            call handle_errors_mdw(sr)
         end if
         !
         flag = .false.
         dom%vegetation = 0
         call prop_get(tmp_ptr, '*', 'Vegetation', flag)
         if (flag) then
            dom%vegetation = 1
         end if
         flag = .false.
         call prop_get(tmp_ptr, '*', 'VegSVNPlants', flag)
         if (flag) then
            dom%vegetation = 2
         end if
         if (dom%vegetation >= 1) then
            call prop_get(tmp_ptr, '*', 'VegHeight', dom%veg_height)
            call prop_get(tmp_ptr, '*', 'VegDiamtr', dom%veg_diamtr)
            call prop_get(tmp_ptr, '*', 'VegDrag', dom%veg_drag)
         end if
         if (dom%vegetation == 1) then
            call prop_get(tmp_ptr, '*', 'VegNstems', dom%veg_nstems)
            !
            ! Read vegetation map
            !
            call prop_get(tmp_ptr, '*', 'VegetationMap', dom%vegfil)
            if (dom%vegfil == '') then
               write (*, *) 'SWAN_INPUT: no vegetation map used for domain ', domainnr
               !call handle_errors_mdw(sr)
            end if
         end if
         !
         sr%output_veg = 0
         if (dom%vegetation > 0) then
            sr%output_veg = 1
         end if
         !
         ! Read directional space
         !
         call prop_get(tmp_ptr, '*', 'NDir', dom%ndir)
         if (dom%ndir < 1) then
            write (*, *) 'SWAN_INPUT: invalid number of directions: ', dom%ndir
            call handle_errors_mdw(sr)
         end if
         parname = ''
         call prop_get(tmp_ptr, '*', 'DirSpace', parname)
         call str_lower(parname, len(parname))
         select case (parname)
         case ('circle')
            dom%dirspace = 1
         case ('sector')
            dom%dirspace = 2
         case default
            if (parname /= '' .or. dom%dirspace < 0) then
               write (*, *) 'SWAN_INPUT: unknown DirSpace: ', parname
               call handle_errors_mdw(sr)
            end if
         end select
         !
         if (dom%dirspace == 2) then
            call prop_get(tmp_ptr, '*', 'StartDir', dom%startdir)
            call prop_get(tmp_ptr, '*', 'EndDir', dom%enddir)
         end if
         !
         ! Read modelled frequency range
         !
         call prop_get(tmp_ptr, '*', 'NFreq', dom%nfreq)
         call prop_get(tmp_ptr, '*', 'FreqMin', dom%freqmin)
         call prop_get(tmp_ptr, '*', 'FreqMax', dom%freqmax)
         !
         ! Read in which domain this domain is nested
         !
         call prop_get(tmp_ptr, '*', 'NestedInDomain', dom%nestnr)
         if (domainnr > 1 .and. &
            & (dom%nestnr < 1 .or. dom%nestnr >= domainnr)) then
            write (*, *) 'SWAN_INPUT: domain', domainnr, ' not nested in a valid domain'
            call handle_errors_mdw(sr)
         end if
         dom%nesfil(1:4) = 'NEST'
         write (dom%nesfil(5:7), '(I3.3)') domainnr
         !
         ! Verify whether quantities should be mapped and optionally be extended
         ! for the current grid.
         !
         call prop_get(tmp_ptr, '*', 'FlowBedLevel', dom%qextnd(q_bath))
         call prop_get(tmp_ptr, '*', 'FlowVegetation', dom%qextnd(q_veg))
         call prop_get(tmp_ptr, '*', 'FlowWaterLevel', dom%qextnd(q_wl))
         call prop_get(tmp_ptr, '*', 'FlowVelocity', dom%qextnd(q_cur))
         call prop_get(tmp_ptr, '*', 'FlowWind', dom%qextnd(q_wind))
         !
         if (dom%qextnd(q_bath) > 0) sr%swmor = .true.
         if (dom%qextnd(q_veg) > 0) sr%swveg = .true.
         if (dom%qextnd(q_wl) > 0) sr%swwlt = .true.
         if (dom%qextnd(q_cur) > 0) sr%swuvt = .true.
         if (dom%qextnd(q_wind) > 0) sr%swwindt = .true.
         !
         if (sr%swuvt) then
            parname = ''
            call prop_get(tmp_ptr, '*', 'FlowVelocityType', parname)
            call str_lower(parname, len(parname))
            select case (parname)
            case ('depth-averaged')
               dom%flowVelocityType = FVT_DEPTH_AVERAGED
            case ('surface-layer')
               dom%flowVelocityType = FVT_SURFACE_LAYER
            case ('wave-dependent')
               dom%flowVelocityType = FVT_WAVE_DEPENDENT
            case (' ')
               !
               ! Default value used
               !
            case default
               write (*, '(a,i0,a)') 'SWAN_INPUT: invalid option for [Domain ', domainnr, '], FlowVelocityType'
               call handle_errors_mdw(sr)
            end select
            !
            ! echo to screen
            !
            select case (dom%flowVelocityType)
            case (FVT_DEPTH_AVERAGED)
               write (*, *) 'Domain ', domainnr, ' is using depth averaged flow velocity.'
            case (FVT_SURFACE_LAYER)
               write (*, *) 'Domain ', domainnr, ' is using surface layer flow velocity.'
            case (FVT_WAVE_DEPENDENT)
               write (*, *) 'Domain ', domainnr, ' is using wave dependent flow velocity.'
            case default
               ! nothing
            end select
         end if
         !
         ! Verify whether output to wavm-file should be written for this domain
         ! (default true)
         !
         call prop_get(tmp_ptr, '*', 'Output', dom%cgnum)
         !
         ! Count number of external forces in current group Domain
         !
         dom%n_extforces = 0
         do ii = 1, size(tmp_ptr%child_nodes)
            node_ptr => tmp_ptr%child_nodes(ii)%node_ptr
            parname = tree_get_name(node_ptr)
            if (parname == 'extforce') then
               dom%n_extforces = dom%n_extforces + 1
            elseif (parname == 'meteofile') then
               dom%n_extforces = dom%n_extforces + 1
               dom%n_meteofiles = dom%n_meteofiles + 1
            end if
         end do
         !
         if (dom%n_extforces > 0) then
            !
            ! Allocate temporary array for names of external forcing files
            !
            allocate (tmp_extforce_names(dom%n_extforces), stat=istat)
            tmp_extforce_names = ''
            !
            ! The input can be read
            !
            par = 0
            do j = 1, size(tmp_ptr%child_nodes)
               node_ptr => tmp_ptr%child_nodes(j)%node_ptr
               parname = tree_get_name(node_ptr)
               if (parname == 'extforce' .or. parname == 'meteofile') then
                  par = par + 1
                  !
                  ! Read value for keyword
                  !
                  call tree_get_data_string(node_ptr, parname, success)
                  !
                  ! Check double occurrences
                  !
                  do ii = 1, par
                     if (tmp_extforce_names(ii) == parname) then
                        write (*, '(a,a,a)') 'SWAN input: Group Domain: External forcing ', trim(parname), ' has already been read'
                        par = par - 1
                        dom%n_extforces = dom%n_extforces - 1
                        if (parname == 'meteofile') dom%n_meteofiles = dom%n_meteofiles - 1
                        exit
                     end if
                  end do
                  !
                  if (tmp_extforce_names(par) == '') then
                     !
                     ! Index par points to empty string, so store the read file name in the array
                     !
                     tmp_extforce_names(par) = trim(parname)
                  end if
                  if (par == dom%n_extforces) then
                     !
                     ! All external forcings read, stop processing keywords
                     !
                     exit
                  end if
               end if
            end do
            !
            ! Allocate array for external forcing files in group Domain
            !
            allocate (dom%extforce_names(dom%n_extforces), stat=istat)
            dom%extforce_names(1:dom%n_extforces) = tmp_extforce_names(1:dom%n_extforces)
            deallocate (tmp_extforce_names)
            !
            if (dom%n_meteofiles > 0) sr%swwindt = .true.
            !
         end if
         !
         ! Check whether also global external forcing files were provided.
         ! If so, the meteofile(s) specified in the DOMAIN group are used.
         !
         if (sr%n_extforces_gen > 0 .and. dom%n_extforces > 0) then
            write (*, '(a,i0)') 'SWAN_INPUT: External forcing files specified in group Domain used instead of those specified in group General for domain ', domainnr
         elseif (sr%n_extforces_gen > 0 .and. dom%n_extforces == 0) then
            !
            ! External forcing files specified in group general will be used for this domain
            !
            dom%n_extforces = sr%n_extforces_gen
            dom%n_meteofiles = sr%n_meteofiles_gen
            !
            ! Allocate array for meteofiles in group Domain
            !
            allocate (dom%extforce_names(dom%n_extforces), stat=istat)
            dom%extforce_names = sr%extforce_names_gen
            !
            if (dom%n_meteofiles > 0) sr%swwindt = .true.
            write (*, '(a,i0)') 'SWAN_INPUT: External forcing files specified in group General used for domain ', domainnr
         end if
         if (dom%n_meteofiles > 0 .and. dom%qextnd(q_wind) > 0) then
            !
            ! User specified wind to be read from COM-file (from FLOW simulation),
            ! but user also specified wind via one or more meteofiles.
            ! The wind from the meteofiles will be used.
            ! These will cover the whole WAVE domain for sure.
            !
            write (*, '(a,i0)') 'SWAN_INPUT: Meteofiles specified in group Domain used instead of meteo input from FLOW for domain ', domainnr
            dom%qextnd(q_wind) = 0
         end if
         !
      end do
      !
      if (sr%swwindt) then
         !
         ! switch on varying wind (varwin) on curvilinear grid (curviwind)
         ! set exclusion value on the default one (excval)
         !
         sr%varwin = .true.
         sr%curviwind = .true.
         sr%excval = -999.0
      end if
      !
      ! determine whether flow data is used
      !
      sr%useflowdata = sr%swmor .or. sr%swwlt .or. sr%swveg .or. sr%swuvt .or. (sr%swwindt .and. dom%n_meteofiles == 0)
      !
      ! determine the number of boundaries
      !
      nbound = 0
      do i = 1, size(mdw_ptr%child_nodes)
         tmp_ptr => mdw_ptr%child_nodes(i)%node_ptr
         parname = tree_get_name(tmp_ptr)
         if (parname == 'boundary') nbound = nbound + 1
      end do
      sr%nbound = nbound
      !
      istat = 0
      allocate (sr%bnd(nbound), stat=istat)
      if (istat /= 0) then
         write (*, *) 'SWAN_INPUT: memory alloc error (nbound)'
         call handle_errors_mdw(sr)
      end if
      !
      sr%specfile = ' '
      do i = 1, nbound
         bnd => sr%bnd(i)
         !
         bnd%parread = -999
         bnd%sshape = -999
         bnd%periodtype = -999
         bnd%dsprtype = -999
         bnd%bndtyp = -999
         bnd%orient = -999
         bnd%turn = -999
         bnd%convar = -999
         bnd%nsect = -999
         bnd%bndcrd_mn = -999
         bnd%bndcrd_xy = -999.0
         bnd%gamma0 = 3.3
         bnd%sigfr = -999.0
         bnd%name = ' '
         nullify (bnd%distance)
         nullify (bnd%waveheight)
         nullify (bnd%period)
         nullify (bnd%direction)
         nullify (bnd%dirspread)
      end do
      !
      ! Optionally find these general quantities in the tables
      !
      if (sr%timedependent) then
         call gettable(sr%tseriesfile, 'General', 'WaveHeight', def_ts_hs, &
                     & 0, errorstring)
         if (def_ts_hs(3) > 1) then
            write (*, *) 'SWAN_INPUT: too many WaveHeight entries in TSeriesFile'
            call handle_errors_mdw(sr)
         end if
         !
         call gettable(sr%tseriesfile, 'General', 'Period', def_ts_tp, &
                     & 0, errorstring)
         if (def_ts_tp(3) > 1) then
            write (*, *) 'SWAN_INPUT: too many Period entries in TSeriesFile'
            call handle_errors_mdw(sr)
         end if
         !
         call gettable(sr%tseriesfile, 'General', 'Direction', def_ts_wd, &
                     & 0, errorstring)
         if (def_ts_wd(3) > 1) then
            write (*, *) 'SWAN_INPUT: too many Direction entries in TSeriesFile'
            call handle_errors_mdw(sr)
         end if
         !
         call gettable(sr%tseriesfile, 'General', 'DirSpreading', def_ts_ds, &
                     & 0, errorstring)
         if (def_ts_ds(3) > 1) then
            write (*, *) 'SWAN_INPUT: too many DirSpreading entries in TSeriesFile'
            call handle_errors_mdw(sr)
         end if
         !
      end if
      !
      boundnr = 0
      do i = 1, size(mdw_ptr%child_nodes)
         bnd_ptr => mdw_ptr%child_nodes(i)%node_ptr
         parname = tree_get_name(bnd_ptr)
         if (parname /= 'boundary') cycle
         boundnr = boundnr + 1
         bnd => sr%bnd(boundnr)
         !
         call prop_get(bnd_ptr, '*', 'Name', bnd%name)
         !
         parname = ''
         call prop_get(bnd_ptr, '*', 'Definition', parname)
         call str_lower(parname, len(parname))
         select case (parname)
         case ('orientation')
            bnd%bndtyp = 1
            !
            parname = ''
            call prop_get(bnd_ptr, '*', 'Orientation', parname)
            call str_lower(parname, len(parname))
            select case (parname)
            case ('n', 'north')
               bnd%orient = 1
            case ('nw', 'northwest')
               bnd%orient = 2
            case ('w', 'west')
               bnd%orient = 3
            case ('sw', 'southwest')
               bnd%orient = 4
            case ('s', 'south')
               bnd%orient = 5
            case ('se', 'southeast')
               bnd%orient = 6
            case ('e', 'east')
               bnd%orient = 7
            case ('ne', 'northeast')
               bnd%orient = 8
            case default
               write (*, *) 'SWAN_INPUT: missing or invalid boundary orientation'
               call handle_errors_mdw(sr)
            end select
            bnd%turn = 1
            !
            parname = ''
            call prop_get(bnd_ptr, '*', 'DistanceDir', parname)
            call str_lower(parname, len(parname))
            select case (parname)
            case ('clockwise')
               bnd%turn = 0
            case ('counter', 'counter-clockwise', ' ')
               bnd%turn = 1
            case default
               write (*, *) 'SWAN_INPUT: invalid distance measurement direction'
               call handle_errors_mdw(sr)
            end select
            !
         case ('grid', 'grid-coordinates')
            bnd%bndtyp = 2
            !
            call prop_get(bnd_ptr, '*', 'StartCoordM', bnd%bndcrd_mn(1))
            call prop_get(bnd_ptr, '*', 'StartCoordN', bnd%bndcrd_mn(2))
            call prop_get(bnd_ptr, '*', 'EndCoordM', bnd%bndcrd_mn(3))
            call prop_get(bnd_ptr, '*', 'EndCoordN', bnd%bndcrd_mn(4))
            !
         case ('xy', 'xy-coordinates')
            bnd%bndtyp = 3
            !
            call prop_get(bnd_ptr, '*', 'StartCoordX', bnd%bndcrd_xy(1))
            call prop_get(bnd_ptr, '*', 'StartCoordY', bnd%bndcrd_xy(2))
            call prop_get(bnd_ptr, '*', 'EndCoordX', bnd%bndcrd_xy(3))
            call prop_get(bnd_ptr, '*', 'EndCoordY', bnd%bndcrd_xy(4))
            !
         case ('fromsp2file')
            bnd%bndtyp = 4
            !
            call prop_get(bnd_ptr, '*', 'OverallSpecfile', sr%specfile)
            write (*, *) 'Boundary conditions from overall 2D spectra file'
            cycle
            !
         case ('fromwwfile')
            bnd%bndtyp = 5
            !
            call prop_get(bnd_ptr, '*', 'WWspecfile', sr%specfile)
            write (*, *) 'Boundary conditions from WAVEWATCH III spectra file'
            cycle
            !
         case default
            write (*, *) 'SWAN_INPUT: missing or invalid boundary orientation definition type'
            call handle_errors_mdw(sr)
         end select
         !
         parname = ''
         call prop_get(bnd_ptr, '*', 'SpectrumSpec', parname)
         call str_lower(parname, len(parname))
         select case (parname)
         case ('from file')
            bnd%parread = 1
            !
            bnd%sshape = 1 ! jonswap
            bnd%periodtype = 1 ! peak
            bnd%dsprtype = 1 ! power
         case ('parametric')
            bnd%parread = 2
            !
            parname = ''
            call prop_get(bnd_ptr, '*', 'SpShapeType', parname)
            call str_lower(parname, len(parname))
            select case (parname)
            case ('jonswap')
               bnd%sshape = 1
            case ('pm', 'pierson-moskowitz')
               bnd%sshape = 2
            case ('gauss')
               bnd%sshape = 3
            case ('bin')
               bnd%sshape = 4
            case default
               write (*, *) 'SWAN_INPUT: missing or invalid boundary spectrum shape type'
               call handle_errors_mdw(sr)
            end select
            !
            parname = ''
            call prop_get(bnd_ptr, '*', 'PeriodType', parname)
            call str_lower(parname, len(parname))
            select case (parname)
            case ('peak')
               bnd%periodtype = 1
            case ('mean')
               bnd%periodtype = 2
            case default
               write (*, *) 'SWAN_INPUT: missing or invalid boundary spectrum period type'
               call handle_errors_mdw(sr)
            end select
            !
            parname = ''
            call prop_get(bnd_ptr, '*', 'DirSpreadType', parname)
            call str_lower(parname, len(parname))
            select case (parname)
            case ('power')
               bnd%dsprtype = 1
            case ('degrees')
               bnd%dsprtype = 2
            case default
               write (*, *) 'SWAN_INPUT: missing or invalid boundary spectrum directional spreading type'
               call handle_errors_mdw(sr)
            end select
            !
         case default
            write (*, *) 'SWAN_INPUT: missing or invalid boundary spectrum specification type'
            call handle_errors_mdw(sr)
         end select
         !
         select case (bnd%sshape)
         case (1) ! jonswap
            call prop_get(bnd_ptr, '*', 'PeakEnhanceFac', bnd%gamma0)
            if (boundnr == 1) then
               ! use this gamm0 instead of the default value
               sr%gamma0 = bnd%gamma0
            end if
         case (3) ! gauss
            call prop_get(bnd_ptr, '*', 'GaussSpread', bnd%sigfr)
         end select
         !
         ! Determine the number of boundary sections
         !
         nsect = 0
         do j = 1, size(bnd_ptr%child_nodes)
            tmp_ptr => bnd_ptr%child_nodes(j)%node_ptr
            parname = tree_get_name(tmp_ptr)
            if (parname == 'condspecatdist') nsect = nsect + 1
         end do
         bnd%nsect = nsect
         bnd%convar = 2
         if (nsect == 0) then
            !
            ! uniform condition
            !
            nsect = nsect + 1
            bnd%convar = 1
         end if
         !
         istat = 0
         allocate (bnd%distance(nsect), stat=istat)
         if (istat == 0) allocate (bnd%waveheight(nsect), stat=istat)
         if (istat == 0) allocate (bnd%period(nsect), stat=istat)
         if (istat == 0) allocate (bnd%direction(nsect), stat=istat)
         if (istat == 0) allocate (bnd%dirspread(nsect), stat=istat)
         if (istat == 0) allocate (bnd%spectrum(nsect), stat=istat)
         if (istat /= 0) then
            write (*, *) 'SWAN_INPUT: memory alloc error (nsect)'
            call handle_errors_mdw(sr)
         end if
         bnd%distance = -999.0
         bnd%waveheight = -999.0
         bnd%period = -999.0
         bnd%direction = -999.0
         bnd%dirspread = -999.0
         bnd%spectrum = ' '
         !
         sectnr = 0
         if (bnd%nsect == 0) then
            !
            ! uniform condition
            !
            sectnr = 1
         end if
         do j = 1, size(bnd_ptr%child_nodes)
            tmp_ptr => bnd_ptr%child_nodes(j)%node_ptr
            parname = tree_get_name(tmp_ptr)
            select case (parname)
            case ('condspecatdist')
               sectnr = sectnr + 1
               call prop_get(tmp_ptr, '*', 'CondSpecAtDist', bnd%distance(sectnr))
            case ('waveheight')
               if (sectnr == 0) then
                  write (*, *) 'SWAN_INPUT: premature wave height specification at ', trim(bnd%name)
                  call handle_errors_mdw(sr)
               end if
               call prop_get(tmp_ptr, '*', 'WaveHeight', bnd%waveheight(sectnr))
            case ('period')
               if (sectnr == 0) then
                  write (*, *) 'SWAN_INPUT: premature period specification at ', trim(bnd%name)
                  call handle_errors_mdw(sr)
               end if
               call prop_get(tmp_ptr, '*', 'Period', bnd%period(sectnr))
            case ('direction')
               if (sectnr == 0) then
                  write (*, *) 'SWAN_INPUT: premature direction specification at ', trim(bnd%name)
                  call handle_errors_mdw(sr)
               end if
               call prop_get(tmp_ptr, '*', 'Direction', bnd%direction(sectnr))
            case ('dirspreading')
               if (sectnr == 0) then
                  write (*, *) 'SWAN_INPUT: premature direction spreading specification at ', trim(bnd%name)
                  call handle_errors_mdw(sr)
               end if
               call prop_get(tmp_ptr, '*', 'DirSpreading', bnd%dirspread(sectnr))
            case ('spectrum')
               if (sectnr == 0) then
                  write (*, *) 'SWAN_INPUT: premature spectrum file specification at ', trim(bnd%name)
                  call handle_errors_mdw(sr)
               end if
               call prop_get(tmp_ptr, '*', 'Spectrum', bnd%spectrum(sectnr))
            end select
         end do
         !
         ! Optionally find these general quantities in the tables
         !
         if (sr%timedependent) then
            call gettable(sr%tseriesfile, bnd%name, 'WaveHeight', bnd%ts_hs, &
                        & 0, errorstring)
            if (bnd%ts_hs(1) < 0) bnd%ts_hs = def_ts_hs
            if (bnd%ts_hs(3) > 1 .and. bnd%ts_hs(3) /= bnd%nsect) then
               write (*, *) 'SWAN_INPUT: invalid number of WaveHeight entries in TSeriesFile'
               call handle_errors_mdw(sr)
            end if
            !
            call gettable(sr%tseriesfile, bnd%name, 'Period', bnd%ts_tp, &
                        & 0, errorstring)
            if (bnd%ts_tp(1) < 0) bnd%ts_tp = def_ts_tp
            if (bnd%ts_tp(3) > 1 .and. bnd%ts_tp(3) /= bnd%nsect) then
               write (*, *) 'SWAN_INPUT: invalid number of Period entries in TSeriesFile'
               call handle_errors_mdw(sr)
            end if
            !
            call gettable(sr%tseriesfile, bnd%name, 'Direction', bnd%ts_wd, &
                        & 0, errorstring)
            if (bnd%ts_wd(1) < 0) bnd%ts_wd = def_ts_wd
            if (bnd%ts_wd(3) > 1 .and. bnd%ts_wd(3) /= bnd%nsect) then
               write (*, *) 'SWAN_INPUT: invalid number of Direction entries in TSeriesFile'
               call handle_errors_mdw(sr)
            end if
            !
            call gettable(sr%tseriesfile, bnd%name, 'DirSpreading', bnd%ts_ds, &
                        & 0, errorstring)
            if (bnd%ts_ds(1) < 0) bnd%ts_ds = def_ts_ds
            if (bnd%ts_ds(3) > 1 .and. bnd%ts_ds(3) /= bnd%nsect) then
               write (*, *) 'SWAN_INPUT: invalid number of DirSpreading entries in TSeriesFile'
               call handle_errors_mdw(sr)
            end if
            !
         end if
         !
      end do
      !
      ! Determine the number of obstacles
      !
      if (obstfil /= ' ') then
         call tree_create_node(mdw_ptr, 'Obstacle Input', obst_ptr)
         call tree_put_data(obst_ptr, transfer(trim(obstfil), node_value), 'STRING')
         call prop_file('ini', trim(obstfil), obst_ptr, istat)
         if (istat /= 0) then
            write (*, *) 'SWAN_INPUT: error reading obstacle file ''', trim(obstfil), ''''
            call handle_errors_mdw(sr)
         end if
         !
         call tree_get_node_by_name(obst_ptr, 'ObstacleFileInformation', tmp_ptr)
         call tree_get_node_by_name(tmp_ptr, 'PolylineFile', pol_ptr)
         if (.not. associated(pol_ptr)) then
            write (*, *) 'SWAN_INPUT: missing PolylineFile keyword in obstacle file'
            call handle_errors_mdw(sr)
         end if
         call tree_get_data_string(pol_ptr, polylinefile, flag)
         call prop_file('tekal', polylinefile, pol_ptr, istat)
         if (istat /= 0) then
            write (*, *) 'SWAN_INPUT: error reading obstacle polygon file ''', trim(polylinefile), ''''
            call handle_errors_mdw(sr)
         end if
         !
         nobst = 0
         nobstpnt = 0
         do i = 1, size(obst_ptr%child_nodes)
            tmp_ptr => obst_ptr%child_nodes(i)%node_ptr
            parname = tree_get_name(tmp_ptr)
            if (parname /= 'obstacle') cycle
            !
            nobst = nobst + 1
            parname = ''
            call prop_get(tmp_ptr, '*', 'Name', parname)
            call tree_get_node_by_name(pol_ptr, parname, tmp_ptr)
            if (.not. associated(tmp_ptr)) then
               write (*, *) 'SWAN_INPUT: obstacle polygon ''', trim(parname), ''' not found'
               call handle_errors_mdw(sr)
            end if
            !
            call tree_get_data_ptr(tmp_ptr, data_ptr, parname)
            nobstpnt = nobstpnt + transfer(data_ptr, nobstpnt)
         end do
         !
      else
         nobst = 0
         nobstpnt = 0
      end if
      sr%nobst = nobst
      sr%nscr = nobstpnt
      !
      istat = 0
      allocate (sr%trane(nobst), stat=istat)
      if (istat == 0) allocate (sr%f(nobst), stat=istat)
      if (istat == 0) allocate (sr%obet(nobst), stat=istat)
      if (istat == 0) allocate (sr%ogam(nobst), stat=istat)
      if (istat == 0) allocate (sr%reflection(nobst), stat=istat)
      if (istat == 0) allocate (sr%refl_type(nobst), stat=istat)
      if (istat == 0) allocate (sr%refl_coeff(nobst), stat=istat)
      if (istat == 0) allocate (sr%nlin(nobst), stat=istat)
      if (istat == 0) allocate (sr%xpob(nobstpnt), stat=istat)
      if (istat == 0) allocate (sr%ypob(nobstpnt), stat=istat)
      !
      if (istat /= 0) then
         write (*, *) 'SWAN_INPUT: memory alloc error (nobst)'
         call handle_errors_mdw(sr)
      end if
      !
      if (nobst > 0) then
         obstnr = 0
         obstpnt = 0
         do i = 1, size(obst_ptr%child_nodes)
            tmp_ptr => obst_ptr%child_nodes(i)%node_ptr
            parname = tree_get_name(tmp_ptr)
            if (parname /= 'obstacle') cycle
            obstnr = obstnr + 1
            !
            parname = ''
            call prop_get(tmp_ptr, '*', 'Type', parname)
            call str_lower(parname, len(parname))
            select case (parname)
            case ('sheet')
               call prop_get(tmp_ptr, '*', 'TransmCoef', sr%trane(obstnr))
            case ('dam')
               sr%trane(obstnr) = 999.9
               call prop_get(tmp_ptr, '*', 'Height', sr%f(obstnr))
               call prop_get(tmp_ptr, '*', 'Alpha', sr%ogam(obstnr))
               call prop_get(tmp_ptr, '*', 'Beta', sr%obet(obstnr))
            end select
            !
            parname = ''
            call prop_get(tmp_ptr, '*', 'Reflections', parname)
            call str_lower(parname, len(parname))
            select case (parname)
            case ('no')
               sr%reflection(obstnr) = 0
               sr%refl_type(obstnr) = 0
            case ('specular')
               sr%reflection(obstnr) = 1
               sr%refl_type(obstnr) = 1
            case ('diffuse')
               sr%reflection(obstnr) = 1
               sr%refl_type(obstnr) = 2
            end select
            !
            if (sr%refl_type(obstnr) > 0) then
               call prop_get(tmp_ptr, '*', 'ReflecCoef', sr%refl_coeff(obstnr))
            end if
            !
            parname = ''
            call prop_get(tmp_ptr, '*', 'Name', parname)
            call tree_get_node_by_name(pol_ptr, parname, tmp_ptr)
            if (.not. associated(tmp_ptr)) then
               write (*, *) 'SWAN_INPUT: obstacle polygon not found'
               call handle_errors_mdw(sr)
            end if
            !
            ! get x,y coordinates ...
            !
            call tree_get_data_ptr(tmp_ptr, data_ptr, parname)
            sr%nlin(obstnr) = transfer(data_ptr, nobstpnt)
            !
            do io = 1, sr%nlin(obstnr)
               obstpnt = obstpnt + 1
               write (parname, '(a,i0)') 'row_', io
               call tree_get_node_by_name(tmp_ptr, parname, node_ptr)
               call tree_get_data_ptr(node_ptr, data_ptr, parname)
               !
               xy = transfer(data_ptr, xy(1), 2)
               sr%xpob(obstpnt) = xy(1)
               sr%ypob(obstpnt) = xy(2)
            end do
         end do
      end if
      !
      ! In paths:
      ! Forward slash works fine on both Windows and Linux
      ! Backward slash does not work on Linux
      slash_ok = 47 ! /
      slash_er = 92 ! \
      call replace_char(sr%flowgridfile, slash_er, slash_ok)
      do i = 1, sr%nloc
         call replace_char(sr%pntfilnam(i), slash_er, slash_ok)
      end do
      !
      write (*, *) 'Done reading input'
      !
      return
999   continue
   end subroutine read_keyw_mdw
!
!
!==============================================================================
   subroutine handle_errors_mdw(sr)
      implicit none
      type(swan_type) :: sr

      !
      if (sr%inputtemplatefile /= '') then
         ! use existing INPUT, mdw keywords not needed
         continue
      else
         ! not using existing INPUT, mdw keywords required
         call wavestop(1, "ERROR while reading keyword based mdw file")
      end if
   end subroutine handle_errors_mdw
!
!
!==============================================================================
   subroutine write_swan_input(sr, itide, calccount, inest, xymiss, wavedata)
      use precision_basics
      !
      implicit none
      !
      integer :: itide
      integer :: inest
      integer :: calccount
      real :: wdir
      real :: wvel
      real(hp) :: xymiss
      character(37) :: curlif
      type(swan_type) :: sr
      type(wave_data_type) :: wavedata
      !
      curlif = sr%dom(inest)%curlif(1:37)
      wvel = sr%wvel(itide)
      wdir = sr%wdir(itide)
      !
      if (sr%inputtemplatefile /= '') then
         call update_swan_inp(sr%inputtemplatefile, itide, sr%nttide, calccount, inest, sr, wavedata)
      else
         call write_swan_inp(wavedata, calccount, &
                      & itide, sr%nttide, inest, sr%nnest, sr%swuvt, &
                      & sr%swuvi, sr%prname, sr%prnumb, sr%title1, sr%title2, &
                      & sr%title3, sr%itest, sr%itrace, &
                      & sr%inrhog, wvel, wdir, &
                      & sr%wlevelcorr, sr%grav, sr%rho, sr%nobst, sr%nscr, &
                      & sr%wfil, sr%ffil, sr%xw, &
                      & sr%yw, sr%alpw, sr%mxw, sr%myw, sr%dxw, &
                      & sr%dyw, sr%trane, sr%f, &
                      & sr%ogam, sr%obet, sr%xpob, sr%ypob, sr%nlin, &
                      & sr%varwin, sr%varfri, sr%ncurv, sr%ncrv, sr%nclin, &
                      & sr%xpcu, sr%ypcu, xymiss, curlif, sr%casl, &
                      & sr%cdd, sr%css, sr%sferic, sr)
      end if
      !

   end subroutine write_swan_input
!
!
!==============================================================================
! open existing INPUT file
! open new INPUT file
! read line by line the existing INPUT file
! write that line to the new input file
   subroutine update_swan_inp(filnam, itide, nttide, calccount, inest, sr, wavedata)
      use precision_basics
      use wave_data
      implicit none

! Global variables
      integer, intent(in) :: calccount
      integer, intent(in) :: itide
      integer, intent(in) :: nttide
      character(*), intent(in) :: filnam
      type(swan_type) :: sr
      type(wave_data_type) :: wavedata
      integer, intent(in) :: inest
!
! Local variables
!
      integer :: old_input
      integer :: new_input
      integer :: loc_tag
      integer :: ierr
      character(256) :: rec
      character(256) :: line
      character(15) :: tbegc
      character(15) :: tendc
      character(15), external :: datetime_to_string
      character(256) :: fname
!
!! executable statements -------------------------------------------------------
!
      write (*, '(2a)') 'Updating pre-existing INPUT file: ', trim(filnam)
      !
      open (newunit=old_input, file=filnam, form='formatted', status='old', iostat=ierr)
      if (ierr /= 0) then
         write (*, '(2a)') '*** ERROR: Unable to find file ', trim(filnam)
         close (old_input)
         call wavestop(1, 'Unable to find file '//trim(filnam))
      end if
      !
      open (newunit=new_input, file='INPUT', form='formatted', status='replace')

      read (old_input, '(a)', iostat=ierr) rec
      if (ierr /= 0) then
         write (*, '(2a)') '*** ERROR: Unable to read file ', trim(filnam)
         close (old_input)
         close (new_input)
         call wavestop(1, 'Unable to read file '//trim(filnam))
      end if
      do while (ierr == 0)
         !=============================================================================
         !           look for tags: $TSTART$, $TSTOP$, $HOTSTART$, $HOTSAVE$
         !=============================================================================
         !
         !
         line = rec
         loc_tag = index(rec, '$TSTART$')
         if (loc_tag /= 0) then
            tbegc = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec)
            line(loc_tag + 16:) = rec(loc_tag + 8:)
            write (line(loc_tag:loc_tag + 15), '(a)') tbegc
            rec = line
         end if
         loc_tag = index(rec, '$TSTOP$')
         if (loc_tag /= 0) then
            tendc = datetime_to_string(wavedata%time%refdate, wavedata%time%calctimtscale * real(wavedata%time%tscale, hp))
            write (line(loc_tag:loc_tag + 15), '(a)') tendc
         end if
         loc_tag = index(rec, '$HOTSTART$')
         if (loc_tag /= 0) then
            ! check for existence of hotfile
            call create_hotstart_line(inest, fname, line, sr)
         end if
         loc_tag = index(rec, '$HOTSAVE$')
         if (loc_tag /= 0) then
            ! SPEC for netcdf hotfiles, with format hot_inest_date_time.nc
            call create_hotfile_line(fname, inest, line, sr, wavedata)
         end if
         write (new_input, '(a)') line
         line = ' '
         line(1:2) = ' $ '
         !
         read (old_input, '(a)', iostat=ierr) rec
      end do
      close (old_input)
      close (new_input)

   end subroutine update_swan_inp
!
!
!==============================================================================
   subroutine write_swan_inp(wavedata, calccount, &
                   & itide, nttide, inest, nnest, swuvt, &
                   & swuvi, prname, prnumb, title1, title2, &
                   & title3, itest, itrace, &
                   & inrhog, wvel, wdir, &
                   & wlevelcorr, grav, rho, nobst, nscr, &
                   & wfil, ffil, xw, &
                   & yw, alpw, mxw, myw, dxw, &
                   & dyw, trane, f, &
                   & ogam, obet, xpob, ypob, nlin, &
                   & varwin, varfri, ncurv, ncrv, nclin, &
                   & xpcu, ypcu, xymiss, curlif, casl, &
                   & cdd, css, sferic, sr)
      use precision_basics
      use properties
      use read_grids
      use wave_data
      !
      implicit none
!
! Global variables
!
      integer, intent(in) :: calccount
      integer, intent(in) :: inest
      integer, intent(in) :: inrhog
      integer, intent(in) :: itest
      integer, intent(in) :: itide
      integer, intent(in) :: itrace
      integer, intent(in) :: mxw
      integer, intent(in) :: myw
      integer, intent(in) :: ncrv
      integer, intent(in) :: ncurv
      integer, intent(in) :: nnest
      integer, intent(in) :: nobst
      integer, intent(in) :: nscr
      integer, intent(in) :: nttide
      integer, dimension(ncrv), intent(in) :: nclin
      integer, dimension(nobst), intent(in) :: nlin
      logical, intent(in) :: sferic
      logical, intent(in) :: swuvi
      logical, intent(in) :: swuvt
      logical, intent(in) :: varfri
      logical, intent(in) :: varwin
      real, intent(in) :: alpw
      real, intent(in) :: cdd
      real, intent(in) :: css
      real, intent(in) :: dxw
      real, intent(in) :: dyw
      real, intent(in) :: grav
      real, intent(in) :: rho
      real, intent(in) :: wdir
      real, intent(in) :: wlevelcorr
      real, intent(in) :: wvel
      real, intent(in) :: xw
      real, intent(in) :: yw
      real, dimension(ncrv), intent(in) :: xpcu
      real, dimension(ncrv), intent(in) :: ypcu
      real, dimension(nobst), intent(in) :: f
      real, dimension(nobst), intent(in) :: obet
      real, dimension(nobst), intent(in) :: ogam
      real, dimension(nobst), intent(in) :: trane
      real, dimension(nscr), intent(in) :: xpob
      real, dimension(nscr), intent(in) :: ypob
      real(hp), intent(in) :: xymiss
      character(16), intent(in) :: prname
      character(*), intent(in) :: casl
      character(37), intent(in) :: curlif
      character(37), intent(in) :: ffil
      character(37) :: wfil
      character(4), intent(in) :: prnumb
      character(72), intent(in) :: title1
      character(72), intent(in) :: title2
      character(72), intent(in) :: title3
      type(swan_type) :: sr
      type(wave_data_type) :: wavedata
!
! Local variables
!
      integer :: bound
      integer :: cindx
      integer :: convar
      integer :: cs
      integer :: dsprtype
      integer :: i
      integer :: ind
      integer :: indend
      integer :: indstart
      integer :: j
      integer :: jendcrv
      integer :: k
      integer :: kst
      integer :: l
      integer :: lc
      integer :: luninp
      integer :: m
      integer :: mdc1
      integer :: msc
      integer :: mxfr
      integer :: myfr
      integer :: n
      integer :: nb
      integer :: npoints
      integer :: nsect
      integer :: orient
      integer :: parread
      integer :: periodtype
      integer :: rindx
      integer :: sect
      integer :: shape
      integer :: loc
      integer :: nuerr
      logical :: exists
      logical :: frame
      real :: alpb
      real :: alpfr
      real :: dir1
      real :: dir2
      real :: dxb
      real :: dyb
      real :: fhigh
      real :: flow
      real :: xlenfr
      real :: xpfr
      real :: xpb
      real :: ypb
      real :: ylenfr
      real :: ypfr
      character(7), dimension(20) :: varnam1
      character(7), dimension(11) :: varnam2
      character(8) :: casl_short
      character(15) :: tbegc
      character(15) :: tendc
      character(15), external :: datetime_to_string
      character(37) :: botfil
      character(37) :: curfil
      character(37) :: mudfil
      character(37) :: vegfil
      character(37) :: aicefil
      character(60) :: lijn
      character(60) :: outfirst
      character(256) :: line
      character(79) :: pointname
      character(256) :: fname
      character(4) :: copy
      type(swan_bnd), pointer :: bnd
      type(swan_dom), pointer :: dom
      !
      ! Do not add more variables to varnam1
      !
      data varnam1/'HSIGN  ', 'DIR    ', 'TM01   ', 'DEPTH ', 'VELOC ',     &
           &       'TRANSP ', 'DSPR   ', 'DISSIP ', 'LEAK  ', 'QB    ',     &
           &       'XP     ', 'YP     ', 'DIST   ', 'UBOT  ', 'STEEPW',     &
           &       'WLENGTH', 'FORCES ', 'RTP    ', 'PDIR  ', 'WIND  '/
      data varnam2/'TPS    ', 'TM02   ', 'TMM10  ', 'DHSIGN', 'DRTM01',     &
           &       'SETUP  ', 'DISSURF', 'DISWCAP', 'DISBOT', 'DISVEG',     &
           &       'NPLANTS'/
!
!! executable statements -------------------------------------------------------
!
      ! The following output string is optionally used on several locations
      !
      tbegc = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec)
      write (outfirst, '(3a,f8.2,a)') "OUT ", tbegc, " ", sr%nonstat_interval, " MIN"

      dom => sr%dom(inest)
      !
      !     *** additional swan arrays ***
      !
      botfil = 'BOTNOW'
      curfil = 'CURNOW'
      mudfil = 'MUDNOW'
      vegfil = 'VEGNOW'
      aicefil = 'AICENOW'
      !
      if (dom%qextnd(q_wind) > 0 .or. dom%n_extforces > 0) wfil = 'WNDNOW'
      nb = sr%nbound
      !
      mxfr = 0 !swani(11)
      myfr = 0 !swani(12)
      npoints = sr%npoints
      !
      ! Dano modifications nfreq, flow and fhigh for shape=BIN
      ! Only meant for parametric, uniform boundary conditions on all given boundaries
      if (nb >= 1) then
         bnd => sr%bnd(1)
         if (bnd%sshape == 4) then
            dom%nfreq = 2
            dom%freqmin = 1.d0 / bnd%period(1) * 0.9d0
            dom%freqmax = 1.d0 / bnd%period(1) / 0.9d0
         end if
      end if

      msc = dom%nfreq
      mdc1 = dom%ndir
      cs = dom%dirspace
      !
      xpfr = 0.0 !swanr(14)
      ypfr = 0.0 !swanr(15)
      alpfr = 0.0 !swanr(16)
      xlenfr = 0.0 !swanr(17)
      ylenfr = 0.0 !swanr(18)
      !
      flow = dom%freqmin
      fhigh = dom%freqmax
      dir1 = dom%startdir
      dir2 = dom%enddir
      !
      ! *** Swan file is written *****************************************
      !
      !     General project data of SWAN file
      !
      lc = len_trim(casl)
      casl_short = casl(1:8)
      if (lc > 8) then
         write (*, '(5a)') '*** MESSAGE: ''', trim(casl), ''' is truncated to ''', trim(casl_short), ''' in SWAN input file'
         lc = len_trim(casl_short)
      end if
      open (newunit=luninp, file='INPUT', status='replace')
      line = ' '
      line(1:72) =                                           &
       & '$***************************** HEADING ************&
       &*********************'
      write (luninp, '(1X,A)') line
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line(1:9) = 'PROJECT  '
      i = 10
      line(i:i) = ''''''
      line(11:26) = prname
      line(27:30) = '''  '''
      line(31:34) = prnumb
      i = 35
      line(i:i) = ''''''
      line(36:) = ' '
      write (luninp, '(1X,A)') line(1:35)
      i = 1
      line(i:i) = ''''''
      i = 74
      line(i:i) = ''''''
      line(2:73) = title1
      write (luninp, '(5X,A)') line(1:74)
      line(2:73) = title2
      write (luninp, '(5X,A)') line(1:74)
      line(2:73) = title3
      write (luninp, '(5X,A)') line(1:74)
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line = ' '
      line(1:72) =                                           &
       & '$***************************** MODEL INPUT ********&
       &*********************'
      write (luninp, '(1X,A)') line
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line = ' '
      !
      !     Coefficient settings
      !
      line(1:22) = 'SET   LEVEL =         '
      line(23:47) = 'NOR =           DEPMIN = '
      write (line(15:21), '(F6.2)') wlevelcorr
      write (line(29:34), '(F6.2)') sr%northdir
      write (line(48:53), '(F6.2)') sr%depmin
      line(54:55) = ' _'
      write (luninp, '(1X,A)') line
      if (sr%modsim > 0) then
         write (line, '(a,i0,a)') '       MAXMES = 1000   MAXERR = ', sr%maxerr, ' _'
         write (luninp, '(1X,A)') line
      end if
      line = ' '
      line(7:22) = 'GRAV =          '
      line(23:47) = 'RHO =           INRHOG = '
      write (line(15:21), '(F6.2)') grav
      write (line(29:36), '(F8.2)') rho
      write (line(48:53), '(  I6)') inrhog
      line(54:55) = ' _'
      write (luninp, '(1X,A)') line
      line = ' '
      if (sr%nautconv) then
         line(7:11) = 'NAUT '
      else
         line(7:11) = 'CART '
      end if
      write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line = ' '
      !
      ! hotfile= .true.: use hotfile
      ! modsim = 2     : quasi-stationary
      ! modsim = 3     : non-stationary
      !
      if (sr%modsim <= 1) then
         line = ' '
         line(1:10) = 'MODE STAT '
         write (luninp, '(1X,A)') line
      elseif (sr%modsim > 1) then
         line = ' '
         line(1:10) = 'MODE NONST'
         write (luninp, '(1X,A)') line
      else
      end if
      if (sferic) then
         line = ' '
         line(1:11) = 'COORD SPHE '
         write (luninp, '(1X,A)') line
      end if
      !
      !     *** activate Setup in SWAN ***
      !
      if (sr%setup) then
         line = ' '
         line(1:5) = 'SETUP'
         write (luninp, '(1X,A)') line
      end if
      !
      !     Definition computation grid(s). Note: these definitions are
      !     also used in preparing NEST files.
      !
      !     ***                                                 ***
      !     ***  additional commands for CURVI-LINEAR grid      ***
      !     ***                                                 ***
      !
      line = ' '
      line(1:6) = 'CGRID '
      line(7:11) = 'CURV '
      write (line(12:21), '(2(I4,1X))') dom%mxc, dom%myc
      ! Write missing values in exactly the same format as used when writing the grid
      write (line(22:80), '(A,2(E25.17,1X))') 'EXCEPT ', xymiss, xymiss
      line(82:83) = ' _'
      write (luninp, '(1X,A)') line
      line = ' '
      if (cs == 1) then
         line(7:22) = 'CIR             '
      elseif (cs == 2) then
         line(7:11) = 'SEC  '
         write (line(12:32), '(F10.2,1X,F10.2)') dir1, dir2
      else
      end if
      write (line(33:37), '(I4,1X)') mdc1
      write (line(38:59), '(2(F10.2,1X))') flow, fhigh
      write (line(60:64), '(I4,1X)') msc
      write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line = ' '
      !
      !     READING of coordinates CURVILINEAR computational grid
      !
      line(1:13) = 'READ COOR 1. '
      ind = index(curlif, ' ')
      i = 14
      line(i:i) = ''''''
      line(15:14 + ind) = curlif
      line(14 + ind:14 + ind) = ''''''
      line(15 + ind:16 + ind) = ' _'
      line(17 + ind:) = ' '
      write (luninp, '(1X,A)') line
      line = ' '
      line(1:) = ' 4   0   1 FREE'
      write (luninp, '(1X,A)') line
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
      line = ' '
      !
      !     Definition of bottom grid(s)
      !
      lijn = 'INPGRID _'
      write (luninp, '(1X,A)') lijn
      if (dom%curvibot == 1) then
         line(1:18) = 'BOTTOM CURV 0. 0. '
         write (line(19:28), '(2(I4,1X))') dom%mxb, dom%myb
         write (luninp, '(1X,A)') line
      else
         fname = dom%depfil
         call readregulargrid(fname, sferic, xpb, ypb, alpb, &
                            & dom%mxb, dom%myb, dxb, dyb)
         !
         ! poles? no, fences!
         !
         dom%mxb = dom%mxb - 1
         dom%myb = dom%myb - 1
         line(1:11) = 'BOTTOM REG '
         write (line(12:74),                                   &
         &       '(2(F10.2,4X),F6.1,1X,2(I4,1X),2(F8.2,1X))')  &
         &       xpb, ypb, alpb,          &
         &       dom%mxb, dom%myb, dxb, dyb
         write (luninp, '(1X,A)') line
         botfil = dom%botfil(1:37)
      end if
      line = ' '
      !
      !     File-name bottom depth  (use temporary file)
      !
      line(1:18) = 'READINP BOTTOM 1.0'
      !     Write (line(16:21),'(F5.1,1X)') fac
      ind = index(botfil, ' ')
      i = 22
      line(i:i) = ''''''
      i = i + 1
      line(i:) = trim(botfil)
      i = i + (ind - 1)
      line(i:i) = ''''''
      i = i + 1
      if (dom%curvibot == 1) then
         line(i:) = ' 4'
      else
         line(i:) = ' 3'
      end if
      i = i + 2
      line(i:) = ' 0 FREE'
      write (luninp, '(1X,A)') trim(line)
      line = ' '
      !
      !     File-name current field (temporary file)
      !
      if (dom%qextnd(q_cur) > 0 .or. swuvi) then
         lijn = 'INPGRID _'
         line(1:18) = 'CURREN CURV 0. 0. '
         write (line(19:28), '(2(I4,1X))') dom%mxc, dom%myc
         write (luninp, '(1X,A)') lijn
         write (luninp, '(1X,A)') trim(line)
         line = ' '
         !
         !        *** read current grid ***
         !
         line(1:21) = 'READ CUR FAC= 1.    _'
         line(22:) = ' '
         write (luninp, '(1X,A)') trim(line)
         line = ' '
         ind = index(curfil, ' ')
         i = 1
         line(i:i) = ''''''
         line(2:) = curfil
         line(ind + 1:ind + 1) = ''''''
         line(ind + 4:ind + 10) = 'IDLA=4 '
         line(ind + 11:ind + 14) = 'FREE'
         line(ind + 15:79) = ' '
         write (luninp, '(1X,A)') trim(line)
         line = ' '
      end if
!-----------------------------------------------------------------------
      !
      !     Definition of sea ice fraction
      !
      if (sr%icedamp == 2) then
         write (luninp, '(1X,A)') '$'
         line = ' '
         lijn = 'INPGRID _'
         line(1:18) = 'AICE CURV 0. 0. '
         write (line(19:28), '(2(I4,1X))') dom%mxc, dom%myc
         write (luninp, '(1X,A)') lijn
         write (luninp, '(1X,A)') trim(line)
         line = ' '
         !
         !     File-name sea ice fraction (use temporary file)
         !
         line = 'READINP AICE 1.0 '''//trim(aicefil)//''' 4 0 FREE'
         write (luninp, '(1X,A)') trim(line)
         line = ' '
      end if
!-----------------------------------------------------------------------
      !
      !     Fluid Mud
      !
      if (wavedata%mode == flow_mud_online) then
         write (luninp, '(1X,A)') '$'
         lijn = 'INPGRID _'
         line(1:18) = 'MUDL CURV 0. 0. '
         write (line(19:28), '(2(I4,1X))') dom%mxc, dom%myc
         write (luninp, '(1X,A)') lijn
         write (luninp, '(1X,A)') trim(line)
         line = ' '
         !
         !     File-name mud depth (use temporary file)
         !
         line = 'READINP MUDL 1.0 '''//trim(mudfil)//''' 4 0 FREE'
         write (luninp, '(1X,A)') trim(line)
         line = ' '
      end if
!-----------------------------------------------------------------------
      !
      !     Vegetation map
      !
      if (dom%vegetation == 1) then
         if (dom%vegfil /= '') then
            write (luninp, '(1X,A)') '$'
            lijn = 'INPGRID _'
            line(1:19) = 'NPLANTS CURV 0. 0. '
            write (line(20:29), '(2(I4,1X))') dom%mxc, dom%myc
            write (luninp, '(1X,A)') lijn
            write (luninp, '(1X,A)') trim(line)
            line = ' '
            !
            !     File-name vegetation map (use temporary file)
            !
            line = 'READINP NPLANTS 1.0 '''//trim(vegfil)//''' 4 0 FREE'
            write (luninp, '(1X,A)') trim(line)
         end if
         line = ' '
         line(1:10) = 'VEGETATION'
         write (line(15:), '(F6.2,1X,F7.4,1X,1I4,1X,F6.2)') dom%veg_height, dom%veg_diamtr, dom%veg_nstems, dom%veg_drag
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      !
      if (dom%vegetation == 2) then
         write (luninp, '(1X,A)') '$'
         lijn = 'INPGRID _'
         line(1:19) = 'NPLANTS CURV 0. 0. '
         write (line(20:29), '(2(I4,1X))') dom%mxc, dom%myc
         write (luninp, '(1X,A)') lijn
         write (luninp, '(1X,A)') trim(line)
         line = ' '
         !
         !     File-name vegetation map (use temporary file)
         !
         line = 'READINP NPLANTS 1.0 '''//trim(vegfil)//''' 4 0 FREE'
         write (luninp, '(1X,A)') trim(line)
         line = ' '
         line(1:10) = 'VEGETATION'
         write (line(15:), '(F6.2,1X,F7.4,1X,A,1X,F6.2)') sr%veg_height, sr%veg_diamtr, "1", dom%veg_drag
         write (luninp, '(1X,A)') line
         line = ' '
      end if
!-----------------------------------------------------------------------
      !
      !     diffraction
      !
      if (sr%diffraction == 1) then
         write (luninp, '(1X,A)') '$'
         line(1:7) = 'DIFFRAC'
         write (line(9:9), '(I1)') sr%diffraction
         write (line(11:20), '(F10.5)') sr%diffr_coeff
         write (line(22:25), '(I4)') sr%diffr_smsteps
         write (line(27:27), '(I1)') sr%diffr_adapt_propag
         write (luninp, '(1X,A)') trim(line)
         line = ' '
      end if
!-----------------------------------------------------------------------
      !
      !     definition of grid for wind field
      !
      if (dom%qextnd(q_wind) > 0 .or. dom%n_extforces > 0) then
         !        *** definition of grid ***
         !
         write (luninp, '(1X,A)') '$'
         if (.not. sr%curviwind) then
            lijn = 'INPGRID _'
            line = ' '
            line(1:7) = 'WIND   '
            write (line(8:35), '(2(F10.2,4X))') xw, yw
            write (line(36:43), '(  F7.1 ,1X) ') alpw
            write (line(44:53), '(2(I4   ,1X))') mxw, myw
            write (line(54:71), '(2(F8.2 ,1X))') dxw, dyw
            write (luninp, '(1X,A)') lijn
            write (luninp, '(1X,A)') trim(line)
            line = ' '
         else
            lijn = 'INPGRID _'
            line = ' '
            line(1:18) = 'WIND CURV 0. 0.   '
            write (line(19:28), '(2(I4,1X))') dom%mxc, dom%myc
            if (sr%excval > -998.99 .or. sr%excval < -999.01) then
               line(29:37) = ' EXCVAL '
               write (line(38:49), '(F12.4)') sr%excval
               line(50:) = ' '
            else
               line(29:) = ' '
            end if
            write (luninp, '(1X,A)') lijn
            write (luninp, '(1X,A)') trim(line)
            line = ' '
         end if
         !
         !        *** read wind grid ***
         !
         line(1:13) = 'READ WIN FAC='
         write (line(14:25), '(1X,F6.2)') sr%alfawind
         line(26:28) = ' _ '
         line(29:) = ' '
         write (luninp, '(1X,A)') trim(line)
         line = ' '
         ind = index(wfil, ' ')
         i = 1
         line(i:i) = ''''''
         line(2:) = wfil
         line(ind + 1:ind + 1) = ''''''
         line(ind + 4:ind + 10) = 'IDLA=4 '
         line(ind + 11:ind + 14) = 'FREE'
         line(ind + 15:79) = ' '
         write (luninp, '(1X,A)') trim(line)
         ! The WU drag formulation is used to be backwards compatible
         write (luninp, '(1X,A)') 'WIND DRAG WU'
         line(1:79) = ' '
      end if
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line = ' '
      !
      !     Uniform wind velocity and direction
      !
      if (.not. varwin .and. dom%n_extforces == 0) then
         if (sr%genmode == 0) then
            line = ' '
            write (luninp, '(1X,A)') line
         elseif (abs(wvel) > 0.) then
            line(1:10) = 'WIND  VEL='
            write (line(11:20), '(F10.2)') wvel
            line(21:25) = ' DIR='
            write (line(26:35), '(F10.2)') wdir
            !line(36:37)   = ' '

            line(36:) = ' DRAG WU'

            write (luninp, '(1X,A)') line
         else
         end if
      end if
!-----------------------------------------------------------------------
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line = ' '
      !
      !     definition of grid for friction field
      !
      if (varfri) then
         lijn = 'INPGRID _'
         line = ' '
         line(1:18) = 'FRIC CURV 0. 0.   '
         write (line(19:28), '(2(I4,1X))') dom%mxc, dom%myc
         if (sr%excval > -998.99 .or. sr%excval < -999.01) then
            line(29:37) = ' EXCVAL '
            write (line(38:49), '(F12.4)') sr%excval
            line(50:) = ' '
         else
            line(29:) = ' '
         end if
         write (luninp, '(1X,A)') lijn
         write (luninp, '(1X,A)') trim(line)
         line = ' '
         line(1:20) = 'READ FRI FAC= 1.   _'
         line(21:) = ' '
         write (luninp, '(1X,A)') trim(line)
         ind = index(ffil, ' ')
         i = 1
         line(i:i) = ''''''
         line(2:) = ffil
         line(ind + 1:ind + 1) = ''''''
         line(ind + 4:ind + 10) = 'IDLA=4 '
         line(ind + 11:ind + 14) = 'FREE'
         line(ind + 15:79) = ' '
         write (luninp, '(1X,A)') trim(line)
         line(1:79) = ' '
      end if
!-----------------------------------------------------------------------
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      !
      !     Input wave parameters:
      !     Note that different sides can be chosen (this in contrast with
      !     HISWA in which the boundary conditions are allways described
      !     at x=0
      !
      !
      !     boundary condition:       par/read
      !     also for nested runs      side/segment
      !                                     xy/ij
      !
      rindx = 30 + 4 * nnest
      cindx = 2
      if (inest == 1) then
         do bound = 1, nb
            bnd => sr%bnd(bound)
            if (bnd%bndtyp == 4) then
               line = ' '
               line(1:10) = 'BOUN NEST '
               line(11:11) = ''''''
               ind = index(sr%specfile, ' ') - 1
               line(12:12 + ind) = sr%specfile
               line(12 + ind:12 + ind) = ''''''
               line(12 + ind + 1:12 + ind + 7) = ' CLOSED'
               write (luninp, '(1X,A)') line
               cycle
            elseif (bnd%bndtyp == 5) then
               line = ' '
               line(1:11) = 'BOUN WWIII '
               line(12:12) = ''''''
               ind = index(sr%specfile, ' ') - 1
               line(13:13 + ind) = sr%specfile
               line(13 + ind:13 + ind) = ''''''
               line(13 + ind + 1:13 + ind + 10) = ' FREE OPEN'
               write (luninp, '(1X,A)') line
               cycle
            end if
            line = ' '
            parread = bnd%parread
            !              User specified conditions
            shape = bnd%sshape
            periodtype = bnd%periodtype
            dsprtype = bnd%dsprtype
            line(1:) = 'BOUN SHAPE'
            if (shape == 1) then
               write (line(12:), '(A, 1X, F6.2)') 'JONSWAP', bnd%gamma0
            elseif (shape == 2) then
               line(12:) = 'PM'
            elseif (shape == 3) then
               write (line(12:), '(A, 1X, F6.2)') 'GAUSS', bnd%sigfr
            elseif (shape == 4) then
               line(12:) = 'BIN'
            else
            end if
            if (periodtype == 1) then
               line(27:) = 'PEAK'
            else
               line(27:) = 'MEAN'
            end if
            if (dsprtype == 1) then
               line(32:) = 'DSPR POWER'
            else
               line(32:) = 'DSPR DEGR'
            end if
            if (.not. (shape == 1 .and. bnd%gamma0 == 0.0)) then
               ! something was actually defined
               write (luninp, '(1X,A)') line
            end if
            !
            line = ' '
            line(1:) = 'BOUN'
            nsect = bnd%nsect
            convar = bnd%convar
            if (bnd%bndtyp == 1) then
               !              Side
               line(6:) = 'SIDE'
               orient = bnd%orient
               if (orient == 1) then
                  line(11:) = 'N'
               elseif (orient == 2) then
                  line(11:) = 'NW'
               elseif (orient == 3) then
                  line(11:) = 'W'
               elseif (orient == 4) then
                  line(11:) = 'SW'
               elseif (orient == 5) then
                  line(11:) = 'S'
               elseif (orient == 6) then
                  line(11:) = 'SE'
               elseif (orient == 7) then
                  line(11:) = 'E'
               elseif (orient == 8) then
                  line(11:) = 'NE'
               else
               end if
               if (bnd%turn == 1) then
                  line(14:) = 'CCW'
               else
                  line(14:) = 'CLOCKW'
               end if
               line(22:) = '_'
               write (luninp, '(1X,A)') line
            else
               !
               ! Segment
               !
               line(6:) = 'SEGM'
               if (bnd%bndtyp == 3) then
                  line(11:) = 'XY'
                  write (line(14:), '(4(F16.7,X))')          &
                       & bnd%bndcrd_xy(1), bnd%bndcrd_xy(2), &
                       & bnd%bndcrd_xy(3), bnd%bndcrd_xy(4)
               elseif (bnd%bndtyp == 2) then
                  line(11:) = 'IJ'
                  write (line(14:), '(4I10)')                          &
                       & bnd%bndcrd_mn(1), bnd%bndcrd_mn(2), &
                       & bnd%bndcrd_mn(3), bnd%bndcrd_mn(4)
               end if
               line(83:) = '&'
               write (luninp, '(1X,A)') line
               rindx = rindx + 4
            end if
            line = ' '
            if (convar == 1) then
               !                 Constant conditions
               line(21:) = 'CON'
               if (parread == 2) then
                  !                    User specified conditions
                  write (line(26:), '(A,4F8.2)') 'PAR ',                 &
                                  & bnd%waveheight(1), bnd%period(1), &
                                  & bnd%direction(1), bnd%dirspread(1)
               else
                  !                    Read conditions from file
                  ind = index(bnd%spectrum(1), ' ') - 1
                  line(26:30) = 'FILE '
                  i = 31
                  line(i:i) = ''''''
                  line(i + 1:i + ind) = bnd%spectrum(1)
                  i = i + ind
                  line(i + 1:i + 1) = ''''''
                  line(i + 3:i + 4) = ' 1'
               end if
               write (luninp, '(1X,A)') line
               rindx = rindx + 5
               cindx = cindx + 1
            else
               !                 Variable conditions
               line(21:) = 'VAR'
               if (parread == 2) then
                  !                    User specified conditions
                  line(25:) = 'PAR'
                  do sect = 1, nsect
                     write (line(29:), '(5F9.2)') bnd%distance(sect), &
                          & bnd%waveheight(sect), bnd%period(sect), &
                          & bnd%direction(sect), bnd%dirspread(sect)
                     if (sect < nsect) then
                        line(75:) = '&'
                     end if
                     rindx = rindx + 5
                     write (luninp, '(1X,A)') line
                     line = ' '
                  end do
                  cindx = cindx + nsect
               else
                  !                    Read conditions from file
                  line(25:29) = 'FILE '
                  do sect = 1, nsect
                     write (line(30:38), '(F9.2)') bnd%distance(sect)
                     line(39:39) = ' '
                     i = 40
                     line(i:i) = ''''''
                     ind = index(bnd%spectrum(sect), ' ') - 1
                     line(i + 1:i + ind) = bnd%spectrum(sect)
                     i = i + ind
                     line(i + 1:i + 1) = ''''''
                     line(i + 3:i + 4) = ' 1'
                     if (sect < nsect) then
                        line(79:) = '&'
                     end if
                     rindx = rindx + 5
                     cindx = cindx + 1
                     write (luninp, '(1X,A)') line
                     line(1:79) = ' '
                  end do
               end if
            end if
         end do
      else
         line = ' '
         line(1:10) = 'BOUN NEST '
         line(11:11) = ''''''
         line(12:15) = 'NEST'
         write (line(16:18), '(I3.3)') inest
         line(19:19) = ''''''
         line(20:26) = ' CLOSED'
         write (luninp, '(1X,A)') line
      end if
!-----------------------------------------------------------------------
      line = ' '
      !
      ! hotfile= true: use hotfile
      !
      if (sr%hotfile) then
         call create_hotstart_line(inest, fname, line, sr)
         write (luninp, '(1X,A)') line
      end if
      !
!-----------------------------------------------------------------------
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line = ' '
      if (sr%genmode == 1) then
         line(1:8) = 'GEN1 '
      elseif (sr%genmode == 2) then
         line(1:8) = 'GEN2 '
      elseif (sr%genmode == 3) then
         if (sr%whitecap == 2) then
            line = 'GEN3 WESTH'
         else
            line(1:8) = 'GEN3 '
         end if
      else
      end if
      write (luninp, '(1X,A)') line
      line = ' '
      if (.not. sr%breaking) then
         line(1:9) = 'OFF BREAK'
         line(10:) = ' '
         write (luninp, '(1X,A)') line
         line = ' '
      else
         line(1:9) = 'BREAK CON'
         write (line(15:30), '(2(F6.2,2X))') sr%cfbr1, sr%cfbr2
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      if (sr%frictype == 1) then
         line(1:9) = 'FRIC JON '
         write (line(15:22), '(F7.4,1X)') sr%frcof
      elseif (sr%frictype == 2) then
         line(1:9) = 'FRIC COLL'
         write (line(15:22), '(F7.4,1X)') sr%frcof
      elseif (sr%frictype == 3) then
         line(1:9) = 'FRIC MAD '
         write (line(15:22), '(F7.4,1X)') sr%frcof
      else
      end if
      if (sr%frictype /= 0) then
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      if (wavedata%mode == flow_mud_online) then
         !
         ! In the future, (some of) the following parameters must be read from the mud-comfile
         !
         !write (luninp, '(1x,a)') 'MUD alpha=1. rhom=1300. nu=0.0027'
         write (luninp, '(1x,a,f8.2,a,f9.6)') 'MUD alpha=1. rhom=', sr%rhomud, &
              & ' nu=', sr%viscmud
      end if
      if (sr%triads) then
         line(1:6) = 'TRIAD '
         write (line(15:41), '(a,F7.4,a,F7.4)') 'trfac=', sr%cftriad1, ' cutfr=', sr%cftriad2
         line(44:66) = ' urcrit=0.2 urslim=0.01'
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      if (.not. sr%windgrowth) then
         line(1:10) = 'OFF WINDG '
         line(11:) = ' '
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      if (.not. sr%quadruplets) then
         line(1:10) = 'OFF QUAD  '
         line(11:) = ' '
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      if (sr%whitecap == 0) then
         line(1:10) = 'OFF WCAP  '
         line(11:) = ' '
         write (luninp, '(1X,A)') line
      else if (sr%whitecap == 1) then
         line(1:20) = 'WCAP KOMEN delta=0  '
         write (luninp, '(1X,A)') line
         !else
         !   line(1:20)  = 'WCAP   CSM   4   2  '
         !   write (luninp, '(1X,A)') line
      end if
      line = ' '
      line(1:10) = 'LIM  10 1 '
      write (luninp, '(1X,A)') line
      line = ' '
      if (.not. sr%fshift) then
         line(1:10) = 'OFF FSHIFT'
         line(11:) = ' '
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      if (dom%vegetation == 1) then
         line(1:10) = 'VEGETATION'
         write (line(15:), '(F6.2,1X,F7.4,1X,I4,1X,F7.4)') dom%veg_height, dom%veg_diamtr, dom%veg_nstems, dom%veg_drag
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      if (sr%icedamp == 2) then
         write (luninp, '(1X,A,1X,E12.4,1X,E12.4,1X,E12.4,1X,E12.4,1X,E12.4,1X,E12.4,1X,E12.4)') 'IC4M2 0.0', sr%icecoeff
         write (luninp, '(1X,A,1X,F7.4)') 'SET ICEWIND', sr%icewind
      end if
      if (sr%modsim == 3 .or. sr%num_scheme == 2) then
         write (luninp, '(1X,A)') 'PROP BSBT'
      end if
      !
      !     Numerical variables
      !
      line(1:47) = 'NUM DIR cdd=        SIGIM css=                 '
      write (line(14:19), '(F6.2)') cdd
      write (line(32:37), '(F6.2)') css
      line(48:) = ' '
      write (luninp, '(1X,A)') trim(line)
      line = ' '
      if (.not. sr%refraction) then
         line(1:10) = 'OFF REFRAC'
         line(11:) = ' '
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line = ' '
      line(1:10) = 'NUM ACCUR '
      if (sr%modsim /= 3) then
         if (sr%alfa > 0.0) then
            write (line(15:), '(F8.3,1X,F8.3,1X,F8.3,1X,F8.3,1X,A,1X,I4,1X,F8.3)') &
                 & sr%drel, sr%dh_abs, sr%dt_abs, sr%percwet, 'STAT', sr%itermx, sr%alfa
         else
            write (line(15:), '(F8.3,1X,F8.3,1X,F8.3,1X,F8.3,1X,I4)') &
            & sr%drel, sr%dh_abs, sr%dt_abs, sr%percwet, sr%itermx
         end if
      else
         write (line(15:), '(F8.3,1X,F8.3,1X,F8.3,1X,F8.3,1X,A,1X,I4)') &
         & sr%drel, sr%dh_abs, sr%dt_abs, sr%percwet, &
         & 'NONSTAT', sr%itermx
      end if
      write (luninp, '(1X,A)') trim(line)
!-----------------------------------------------------------------------
      line = ' '
      !
      !     Obstacles
      !
      if (nobst > 0) then
         l = 0
         do i = 1, nobst
            line = ' '
            if (trane(i) < 999.0) then
               line(1:11) = 'OBST TRANS '
               write (line(12:17), '(F6.3)') trane(i)
            else
               line(1:9) = 'OBST DAM '
               write (line(10:16), '(F6.2,1X)') f(i)
               write (line(17:23), '(F6.2,1X)') ogam(i)
               write (line(24:30), '(F6.2,1X)') obet(i)
            end if
            if (sr%reflection(i) == 1) then
               write (line(31:45), '(a,F9.6)') ' REFL ', sr%refl_coeff(i)
               if (sr%refl_type(i) == 1) then
                  write (line(46:51), '(a)') ' RSPEC'
               elseif (sr%refl_type(i) == 2) then
                  !
                  ! With SWAN version 40.51A, 'RDIFF 1 1 1' is replaced with 'RDIFF 1'
                  ! Not backwards compatible!
                  !
                  write (line(46:53), '(a)') ' RDIFF 1'
               else
                  !
                  ! Wrong type indicator
                  !
               end if
            end if
            line(57:62) = ' LIN _'
            write (luninp, '(1X,A)') line
            line = ' '
            do j = 1, nlin(i)
               line = ' '
               l = l + 1
               write (line(13:70), '(2(E25.17,4X))') xpob(l), ypob(l)
               if (j /= nlin(i)) line(72:72) = '_'
               write (luninp, '(1X,A)') line
               line = ' '
            end do
         end do
      end if
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
      line = ' '
      line(1:72) = '$***************************** OUTPUT REQUEST **************************'
      write (luninp, '(1X,A)') line
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      !
      ! The following line avoids "****" being written in the spectral files due to format errors
      !
      write (luninp, '(A,I0)') 'OUTPUT OPTIONS SPEC ndec=', sr%ndec
      do i = 1, size(varnam1)
         write (luninp, '(1X,3A)') 'QUANTITY ', varnam1(i), ' excv=-999.0'
      end do
      do i = 1, size(varnam2)
         write (luninp, '(1X,3A)') 'QUANTITY ', varnam2(i), ' excv=-999.0'
      end do
      if (allocated(sr%add_out_names)) then
         do i = 1, size(sr%add_out_names)
            write (luninp, '(1X,3A)') 'QUANTITY ', sr%add_out_names(i), ' excv=-999.0'
         end do
      end if
      write (luninp, '(1X,A)') '$ '
      line = ' '
      !
      !     *** output definitions ***
      !
      !     definition of nested grid
      !
      if (nnest > 1) then
         if (inest < nnest) then
            do kst = 2, nnest
               if (sr%dom(kst)%nestnr == inest) then
                  line(1:7) = 'POINTS '
                  i = 8
                  line(i:i) = ''''''
                  fname(1:5) = 'NGRID'
                  write (fname(6:8), '(I3.3)') kst
                  line(9:16) = fname(1:8)
                  i = 17
                  line(i:i) = ''''''
                  line(18:23) = ' FILE '
                  i = 24
                  line(i:i) = ''''''
                  line(25:31) = 'SWANIN_'
                  line(32:39) = fname(1:8)
                  i = 40
                  line(i:i) = ''''''
                  line(41:79) = ' '
                  write (luninp, '(1X,A)') line
                  line = ' '
                  line(1:6) = 'SPEC _'
                  line(7:) = ' '
                  write (luninp, '(1X,A)') line
                  line = ' '
                  i = 1
                  line(i:i) = ''''''
                  line(2:9) = fname(1:8)
                  i = 10
                  line(i:i) = ''''''
                  line(12:22) = 'SPEC2D  ABS'
                  i = 24
                  line(i:i) = ''''''
                  line(25:31) = sr%dom(kst)%nesfil(1:7)
                  i = 32
                  line(i:i) = ''''''
                  write (luninp, '(1X,A)') line
               end if
            end do
         end if
      end if
      !
      ! Table output
      ! The length of a line in the SWAN output is limited to 360 characters.
      ! Splitting the output solves this problem.
      !
      line(1:6) = 'TABLE '
      i = 7
      line(i:i) = ''''''
      line(8:15) = 'COMPGRID'
      i = 16
      line(i:i) = ''''''
      line(17:24) = ' NOHEAD '
      i = 25
      line(i:i) = ''''''
      line(26:33) = 'SWANOUT1'
      i = 34
      line(i:i) = ''''''
      line(35:37) = '  _'
      line(38:) = ' '
      write (luninp, '(1X,A)') line
      line = ' '
      if (calccount == 1 .and. sr%modsim == 3) then
         ! The following do-loop is used to write an underscore (_) at the end of the last line with varnam elements
         ! Is there a more easy way?
         !
         do j = 1, ceiling(real(size(varnam1)) / 6.0)
            m = min(6, size(varnam1) - (j - 1) * 6)
            lijn = ' '
            write (lijn, '(A,I1.1,A)') "(", m, "(2X,A),'_')"
            write (luninp, lijn) (varnam1(n), n=(j - 1) * 6 + 1, min(j * 6, size(varnam1)))
         end do
         write (luninp, '(2A)') "  ", trim(outfirst)
      else
         write (luninp, '(6(2X,A),''_'')') varnam1
      end if
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      !
      line(1:6) = 'TABLE '
      i = 7
      line(i:i) = ''''''
      line(8:15) = 'COMPGRID'
      i = 16
      line(i:i) = ''''''
      line(17:24) = ' NOHEAD '
      i = 25
      line(i:i) = ''''''
      line(26:33) = 'SWANOUT2'
      i = 34
      line(i:i) = ''''''
      line(35:37) = '  _'
      line(38:) = ' '
      write (luninp, '(1X,A)') line
      line = ' '
      if (calccount == 1 .and. sr%modsim == 3) then
         ! The following do-loop is used to write an underscore (_) at the end of the last line with varnam elements
         ! Is there a more easy way?
         !
         do j = 1, ceiling(real(size(varnam2)) / 6.0)
            m = min(6, size(varnam2) - (j - 1) * 6)
            lijn = ' '
            write (lijn, '(A,I1.1,A)') "(", m, "(2X,A),' _')"
            write (luninp, lijn) (varnam2(n), n=(j - 1) * 6 + 1, min(j * 6, size(varnam2)))
         end do
         write (luninp, '(2A)') "  ", trim(outfirst)
      else
         write (luninp, '(6(2X,A),'' _'')') varnam2
      end if
      line = ' '
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      !
      if (allocated(sr%add_out_names)) then
         line(1:6) = 'TABLE '
         i = 7
         line(i:i) = ''''''
         line(8:15) = 'COMPGRID'
         i = 16
         line(i:i) = ''''''
         line(17:24) = ' NOHEAD '
         i = 25
         line(i:i) = ''''''
         line(26:33) = 'SWANOUT3'
         i = 34
         line(i:i) = ''''''
         line(35:37) = '  _'
         line(38:) = ' '
         write (luninp, '(1X,A)') line
         line = ' '
         write (luninp, '(6(2X,A7),''_'')') sr%add_out_names
         line = ' '
         line(1:2) = '$ '
         write (luninp, '(1X,A)') line
      end if
!-----------------------------------------------------------------------
      line = ' '
      !
      !     Curves
      !
      if (ncurv > 0) then
         k = 0
         do i = 1, ncurv
            line = ' '
            line(1:7) = 'CURVE  '
            j = 8
            line(j:j) = ''''''
            line(9:11) = 'cur'
            write (line(12:13), '(I2.2)') i
            write (line(14:15), '(I2.2)') inest
            j = 16
            line(j:j) = ''''''
            k = k + 1
            write (line(21:56), '(2(F14.6,4X))') xpcu(k), ypcu(k)
            line(57:57) = '_'
            write (luninp, '(1X,A)') line
            line = ' '
            jendcrv = nclin(k)
            do j = 2, jendcrv
               k = k + 1
               line = ' '
               write (line(11:15), '(I5)') nclin(k)
               write (line(21:56), '(2(F14.6,4X))') xpcu(k), ypcu(k)
               !              Modification
               if (j /= jendcrv) line(57:57) = '_'
               write (luninp, '(1X,A)') line
               line = ' '
            end do
            line(1:2) = '$ '
            write (luninp, '(1X,A)') line
            line = ' '
            line(1:7) = 'TABLE  '
            j = 8
            line(j:j) = ''''''
            line(9:11) = 'cur'
            write (line(12:13), '(I2.2)') i
            write (line(14:15), '(I2.2)') inest
            j = 16
            line(j:j) = ''''''
            line(21:26) = 'NOHEAD'
            line(31:31) = ''''''
            line(32:39) = 'SWANOUT_'
            line(40:47) = line(9:16)
            line(51:51) = '_'
            write (luninp, '(1X,A)') line
            line = ' '
            write (luninp, '(4(2X,A),A)') varnam1(11), varnam1(12), varnam1(13),     &
                                        & varnam1(4), ' _'
            write (luninp, '(5(2X,A),A)') varnam1(1), varnam1(3), varnam1(2),        &
                                        & varnam1(7), varnam1(8), ' _'
            write (luninp, '(2(2X,A))') varnam1(16), varnam1(5)
            line = ' '
         end do
      elseif (ncurv == -1) then
         !
         ! Output curves specified in a Tekal file
         ! Handle in a separate subroutine
         !
         write (*, '(1X,A)') ' Output curves are specified in a polyline file'
         call outputCurvesFromFile
      end if
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
      !
      !     Output locations
      !
      line = ' '
      !
      ! loop over the location files
      !
      if (sr%output_points) then
         do loc = 1, sr%nloc
            pointname = get_pointname(sr%pntfilnam(loc))
            pointname = get_pointname(sr%pntfilnam(loc))

            line(1:7) = 'POINTS '
            i = 8
            line(i:i) = ''''''
            line(i + 1:) = pointname
            i = i + 1 + len_trim(pointname)
            line(i:i) = ''''''
            line(i + 1:) = ' _'
            write (luninp, '(1X,A)') line
            line(1:79) = ' '
            if (sr%output_pnt_file) then
               line = "FILE '"//trim(sr%pntfilnam(loc))//"'"
               write (luninp, '(1X,A)') line
               line = ' '
            else
               do n = 1, npoints
                  write (line(1:26), '(E25.17,1X)') sr%xyloc(1, n)
                  write (line(27:52), '(E25.17,1X)') sr%xyloc(2, n)
                  if (n < npoints) then
                     line(54:55) = ' _'
                  end if
                  write (luninp, '(1X,A)') line
                  line(1:79) = ' '
               end do
            end if
            line(1:2) = '$ '
            write (luninp, '(1X,A)') line
            line = ' '
            if (sr%output_table) then
               ! Write the (constructed) name of the tabfile to pntfilnamtab
               ! The indexes indstart and indend are used to flag the region in line containing this name
               ! Result should look like: TABLE 'test' HEAD 'testn05t05000001.tab' XP YP DEP HS DIR RTP TM01
               ! This allows for up to 99 nested wave models
               !
               line(1:6) = 'TABLE '
               i = 7
               line(i:i) = ''''''
               line(i + 1:) = pointname
               i = i + 1 + len_trim(pointname)
               line(i:i) = ''''''
               line(i + 1:) = ' '
               line(i + 2:) = 'HEAD '
               i = i + 7
               line(i:i) = ''''''
               line(i + 1:) = pointname
               indstart = i + 1
               i = i + 1 + len_trim(pointname)
               call add_inest(nnest, line, i, inest)
               if (nttide > 1 .or. wavedata%mode /= stand_alone) then
                  line(i:) = 't'
                  i = i + 1
                  if (nttide > 1) then
                     write (line(i:), '(I8.8)') 1000000 * inest + itide
                  else ! wavedata%mode /= stand_alone
                     write (line(i:), '(I8.8)') calccount
                  end if
                  i = i + 8
               end if
               line(i:) = '.tab'
               i = i + 4
               indend = i - 1
               sr%pntfilnamtab(loc) = line(indstart:indend)
               line(i:i) = ''''''
               line(i + 1:) = ' XP YP DEP HS DIR RTP TM01 _'
               write (luninp, '(1X,A)') line
               line = ' '
               line(37:56) = 'DSPR UBOT WIND VEL  '
               if (sr%icedamp == 2) then
                  line(56:68) = 'AICE DISICE  '
               end if
               write (luninp, '(1X,A)') line
            end if
            line = ' '
            if (sr%output_spec1d) then
               line(1:6) = 'SPEC  '
               i = 7
               line(i:i) = ''''''
               line(i + 1:) = pointname
               i = i + 1 + len_trim(pointname)
               line(i:i) = ''''''
               line(i + 1:) = ' '
               line(i + 2:) = 'SPEC1D '
               i = i + 9
               line(i:i) = ''''''
               line(i + 1:) = sr%pntfilnam(loc)
               i = i + 1 + lc
               call add_inest(nnest, line, i, inest)
               !
               ! Running online with Delft3D-FLOW: itide contains the output counter
               !
               if (nttide > 1 .or. wavedata%mode /= stand_alone) then
                  line(i:) = 't'
                  if (nttide > 1) then
                     write (line(i + 1:), '(I6.6)') itide
                  else ! wavedata%mode /= stand_alone
                     write (line(i + 1:), '(I6.6)') calccount
                  end if
                  i = i + 7
               end if
               line(i:) = '.sp1'
               i = i + 4
               line(i:i) = ''''''
               write (luninp, '(1X,A)') line
               line = ' '
            end if
            if (sr%output_spec2d) then
               line(1:6) = 'SPEC  '
               i = 7
               line(i:i) = ''''''
               line(i + 1:) = pointname
               i = i + 1 + len_trim(pointname)
               line(i:i) = ''''''
               line(i + 1:) = ' '
               line(i + 2:) = 'SPEC2D '
               i = i + 9
               line(i:i) = ''''''
               line(i + 1:) = pointname
               i = i + 1 + len_trim(pointname)
               call add_inest(nnest, line, i, inest)
               !
               ! Running online with Delft3D-FLOW: itide contains the output counter
               !
               if (nttide > 1 .or. wavedata%mode /= stand_alone) then
                  line(i:) = 't'
                  if (nttide > 1) then
                     write (line(i + 1:), '(I6.6)') itide
                  else ! wavedata%mode /= stand_alone
                     write (line(i + 1:), '(I6.6)') calccount
                  end if
                  i = i + 7
               end if
               line(i:) = '.sp2'
               i = i + 4
               line(i:i) = ''''''
               !
               ! When running in non-stationary mode
               ! and when this is the first WAVE calculation of the simulation:
               ! Also produce spectral output for the start time of the simulation
               ! This may be needed by applications using this output as input and
               ! needing to cover the full simulation period.
               !
               if (calccount == 1 .and. sr%modsim == 3) then
                  line(i + 2:) = trim(outfirst)
               end if
               write (luninp, '(1X,A)') line
               line = ' '
            end if
         end do
      end if
      line(1:2) = '$ '
      write (luninp, '(1X,A)') line
      line = ' '
      ! frame not set anywhere!
      frame = .false.
      if (frame) then
         line(1:5) = 'FRAME'
         line(6:6) = ' '
         i = 7
         line(i:i) = ''''''
         line(8:14) = 'clframe'
         i = 15
         line(i:i) = ''''''
         line(16:16) = ' '
         write (line(17:25), '(F8.2 ,1X)') xpfr
         write (line(26:34), '(F8.2 ,1X)') ypfr
         write (line(35:43), '(F8.2 ,1X)') alpfr
         write (line(44:54), '(F10.2,1X)') xlenfr
         write (line(55:65), '(F10.2,1X)') ylenfr
         line(66:67) = '_ '
         write (luninp, '(1X,A)') line
         line = ' '
         write (line(1:9), '(I4,1X,I4)') mxfr, myfr
         write (luninp, '(1X,A)') line
         line = ' '
      end if
      !
      if (sr%hotfile) then
         call create_hotfile_line(fname, inest, line, sr, wavedata)
      end if
      write (luninp, '(1X,A)') line

      !-----------------------------------------------------------------------
      !
      !     Compute and test parameters
      !
      line = ' '
      line(1:35) = 'TEST  ITEST=      ITRACE=          '
      write (line(14:16), '(I3)') itest
      write (line(27:29), '(I3)') itrace
      line(36:) = ' '
      write (luninp, '(1X,A)') line
      line = ' '
      !
      if (.not. sr%compmode) then
         line(1:1) = '$'
         line(2:) = ' '
      else
         !
         ! modsim = 2   : quasi-stationary
         ! modsim = 3   : non-stationary
         !
         line = ' '
         if (sr%modsim <= 1) then
            !
            ! stationary
            !
            line(1:7) = 'COMPUTE'
         elseif (sr%modsim == 2) then
            tendc = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec)
            write (line, '(A,1X,A)') 'COMPUTE STAT  ', tendc
         elseif (sr%modsim == 3) then
            !
            ! non-stationary
            !
            ! starttime
            !
            tbegc = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec)
            !
            ! endtime
            !
            tendc = datetime_to_string(wavedata%time%refdate, wavedata%time%calctimtscale * real(wavedata%time%tscale, hp))
            !
            if (sr%hotfile .and. sr%usehottime /= '00000000.000000') then
               !
               ! A hotfile is being used
               ! SWAN will stop if usehottime is not equal to tbegc
               !
               if (sr%usehottime > tbegc) then
                  write (*, '(5a)') "*** ERROR: Time of hotfile to read (", sr%usehottime, ") is bigger than the simulation start time (", &
                           & tbegc, ")"
                  write (*, '(a,f8.2,a)') "           The non-stationary TimeInterval (", sr%nonstat_interval, ") must be equal to or smaller than the FLOW simulation interval"
                  call wavestop(1, "While preparing SWAN input file")
               end if
            end if
            !
            ! built line
            !
            line = ' '
            line(1:16) = 'COMPUTE NONSTAT '
            write (line(17:31), '(a)') tbegc
            write (line(33:40), '(f8.2)') sr%deltc
            line(41:44) = ' MIN'
            write (line(46:61), '(a)') tendc
         else
         end if
      end if
      write (luninp, '(1X,A)') line
      !
      line = ' '
      line(1:4) = 'STOP'
      line(5:) = ' '
      write (luninp, '(1X,A)') line
      line = ' '
      close (luninp)
      !
      ! keepinput:
      ! keepinput = true : keep a copy of the swan INPUT file
      !
      if (sr%keepinput) then
         copy = 'copy'
         write (fname, '(a,i0,4a)') 'INPUT_', inest, '_', tbegc(1:8), '_', tbegc(10:15)
         call cp_file('INPUT', fname, copy, nuerr)
         if (nuerr > 0) then
            write (*, *) '*** ERROR: While copying INPUT to ', trim(fname), ', errorcode:', nuerr
            call wavestop(1, '*** ERROR: While copying INPUT to ', trim(fname))
         end if
      end if
!
!
!
   contains
!
!
!===============================================================================
      subroutine outputCurvesFromFile()
         use precision_basics
         integer :: i
         integer :: j
         integer :: istat
         real(sp), dimension(1:2) :: inputvals
         character(1), pointer, dimension(:) :: data_ptr
         character(6) :: number
         character(30) :: node_type
         character(30) :: parname
         character(80) :: curname
         character(80) :: line
         type(tree_data), pointer :: cur_ptr
         type(tree_data), pointer :: pol_ptr
         type(tree_data), pointer :: tmp_ptr

         nullify (pol_ptr)
         call tree_create('Delft3D-WAVE output curves', pol_ptr)
         istat = 0
         call prop_file('tekal', trim(sr%curvefil), pol_ptr, istat)
         if (istat /= 0) then
            select case (istat)
            case (1)
               call wavestop(1, '*** ERROR File: '//trim(sr%curvefil)//' not found')
            case (3)
               call wavestop(1, '*** ERROR Premature EOF in file: '//trim(sr%curvefil))
            case default
               call wavestop(1, '*** ERROR Read error from file: '//trim(sr%curvefil))
            end select
         end if
         !
         ! if no line exists in the polyline file
         !
         if (.not. associated(pol_ptr%child_nodes)) then
            write (*, '(1X,A)') ' Error! 0 output curve is specified in the polyline file!'
            call wavestop(1, ' Error! 0 output curve is specified in the polyline file!')
         end if
         do i = 1, size(pol_ptr%child_nodes)
            cur_ptr => pol_ptr%child_nodes(i)%node_ptr
            curname = tree_get_name(cur_ptr)
            line = trim(curname)
            line = line(1:8) ! sname can only be 8 characters long in SWAN source code (subroutine SWTABP)
            write (luninp, '(1x,3a)') 'CURVE  ''', trim(line), '''  _'
            do j = 1, size(cur_ptr%child_nodes)
               tmp_ptr => cur_ptr%child_nodes(j)%node_ptr

               write (parname, '(a,i0)') 'row_', j
               call tree_get_data_ptr(tmp_ptr, data_ptr, node_type)
               !
               ! inputvals is of type real(fp)
               ! the data to be retrieved is in real(sp)
               ! call transfer with a real(sp) constant as second parameter
               !
               inputvals = transfer(data_ptr, 0., 2)
               write (line, '(18x,2(f14.6,4x))') inputvals(1), inputvals(2) ! needed in case of spherical output, was f10.2
               if (j /= 1) then
                  line(14:14) = '1'
               end if
               if (j < size(cur_ptr%child_nodes)) then
                  write (line, '(2a)') trim(line), '    _'
               end if
               write (luninp, '(a)') line
            end do
            if (nttide > 1) then
               write (number, '(I6.6)') itide
            else ! wavedata%mode /= stand_alone
               write (number, '(I6.6)') calccount
            end if
            write (luninp, '(1x,a)') '$ '
            line = trim(curname)
            line = line(1:8) ! sname can only be 8 characters long in SWAN source code
            write (luninp, '(1x,6a)') 'TABLE  ''', trim(line), '''    NOHEAD    ''SWANOUT_', trim(curname), trim(number), '''   _'
            write (luninp, '(4(2X,A),A)') varnam1(11), varnam1(12), varnam1(13),     &
                                       & varnam1(4), ' _'
            write (luninp, '(5(2X,A),A)') varnam1(1), varnam1(3), varnam1(2),        &
                                       & varnam1(7), varnam1(8), ' _'
            write (luninp, '(2(2X,A))') varnam1(16), varnam1(5)
         end do
      end subroutine outputCurvesFromFile

   end subroutine write_swan_inp
!
!
!==============================================================================
   subroutine create_hotfile_line(fname, inest, line, sr, wavedata)
      use precision_basics
      use wave_data
      !
      implicit none
      !
      ! Global variables
      integer, intent(in) :: inest
      character(*) :: fname
      character(256) :: line

      type(swan_type) :: sr
      type(wave_data_type) :: wavedata

      ! Local variables
      character(15), external :: datetime_to_string

      ! Default: put current time in writehottime
      ! writehottime will be overwritten by tendc when quasi-/non-stationary
      !
      sr%writehottime = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec)
      !
      ! hotstart:
      ! hotfile= true: use hotfile
      ! modsim = 2   : quasi-stationary
      ! modsim = 3   : non-stationary
      !
      if (sr%modsim == 2) then
         ! quasi-stationary
         sr%writehottime = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec)
      elseif (sr%modsim == 3) then
         ! non-stationary
         sr%writehottime = datetime_to_string(wavedata%time%refdate, wavedata%time%calctimtscale * real(wavedata%time%tscale, hp))
      else
      end if
      !
      ! line to ensure that SWAN is going to produce a hotfile
      !
      write (fname, '(a,i0,5a)') 'hot_', inest, '_', trim(sr%writehottime(1:8)), '_', trim(sr%writehottime(10:15)), '.nc'
      line = "SPEC 'COMPGRID' RELATIVE '"//trim(fname)//"' MDGRID"

   end subroutine create_hotfile_line
!
!
!==============================================================================
   subroutine create_hotstart_line(inest, fname, line, sr)
      implicit none

      ! Global variables
      integer, intent(in) :: inest
      character(256) :: line
      character(*) :: fname

      type(swan_type) :: sr

      ! Local variables
      character(15), external :: datetime_to_string
      logical :: exists

      line = ' '
      !

      ! define the name of the hotfile to be used
      !
      write (fname, '(a,i0,5a)') 'hot_', inest, '_', trim(sr%usehottime(1:8)), '_', trim(sr%usehottime(10:15)), '.nc'
      !
      ! use it when it exists
      !
      inquire (file=trim(fname), exist=exists)
      if (exists) then
         line = "INITIAL HOTSTART '"//trim(fname)//"' NETCDF"
         write (*, '(2a)') '  Using SWAN hotstart file: ', trim(fname)
      else ! check if there exists at least 1 partioned hotfile
         write (fname, '(a,i0,5a,i3.3,a)') 'hot_', inest, '_', trim(sr%usehottime(1:8)), '_', trim(sr%usehottime(10:15)), '-', 1, '.nc'
         inquire (file=trim(fname), exist=exists)
         if (exists) then
            write (fname, '(a,i0,5a)') 'hot_', inest, '_', trim(sr%usehottime(1:8)), '_', trim(sr%usehottime(10:15)), '.nc' ! swan input needs filename without partition no
            line = 'INITIAL HOTSTART '''//trim(fname)//''''' NETCDF'
            write (*, '(2a)') '  Using SWAN hotstart file: ', trim(fname)
         else
            ! No hotfile, set usehottime to 0.0 to flag that it isn't used
            sr%usehottime = '00000000.000000'
            line = ' $ '
         end if
      end if

   end subroutine create_hotstart_line
!
!
!==============================================================================
   subroutine adjustinput(sr)
      use properties
      implicit none
      !
      type(swan_type) :: sr
      !
      character(256) :: filnam
      character(256) :: parname
      integer :: i
      integer :: in
      integer :: istat
      logical :: exists
      type(tree_data), pointer :: domain_ptr
      type(tree_data), pointer :: input_tree
      type(swan_dom), pointer :: dom
      !
      filnam = trim(sr%filnam)//'.opt'
      inquire (file=trim(filnam), exist=exists)
      if (.not. exists) return
      !
      ! Create input tree
      !
      call tree_create("Delft3D-WAVE input", input_tree)
      istat = 0
      call prop_file('ini', trim(filnam), input_tree, istat)
      if (istat /= 0) return
      !
      sr%append_com = .false.
      call prop_get(input_tree, '*', 'AppendCOM', sr%append_com)
      call prop_get(input_tree, '*', 'checkVersionNumber', sr%checkVersionNumber)
      if (.not. sr%checkVersionNumber) then
         write (*, '(a)') '*** MESSAGE: The check on the SWAN version number is disabled'
      end if
      !
      do i = 1, size(input_tree%child_nodes)
         !
         ! Does input_tree contain a child with name 'domain'?
         !
         domain_ptr => input_tree%child_nodes(i)%node_ptr
         parname = tree_get_name(domain_ptr)
         if (parname /= 'domain') cycle
         parname = ''
         call prop_get(domain_ptr, '*', 'Grid', parname)
         !
         do in = 1, sr%nnest
            if (sr%dom(in)%curlif == parname) exit
         end do
         if (in > sr%nnest) cycle
         dom => sr%dom(in)
         !
         call prop_get(domain_ptr, '*', 'FlowBedLevel', dom%qextnd(q_bath))
         call prop_get(domain_ptr, '*', 'FlowVegetation', dom%qextnd(q_veg))
         call prop_get(domain_ptr, '*', 'FlowWaterLevel', dom%qextnd(q_wl))
         call prop_get(domain_ptr, '*', 'FlowVelocity', dom%qextnd(q_cur))
         call prop_get(domain_ptr, '*', 'FlowWind', dom%qextnd(q_wind))
         !
      end do
      !
   end subroutine adjustinput
!
!
!==============================================================================
!> pointname is pntfilnam without path, spaces and extension
   function get_pointname(pntfilnam) result(pointname)
      character(len=*), intent(in) :: pntfilnam !< input filename
      character(len=79) :: pointname !< function result

      integer :: indstart, indend
      integer, parameter :: maxPointNameLength = 8 ! max length for sname in swanpre2

      !
      ! Remove the path, spaces and extension from pntfilnam
      !
      indstart = max(0, index(pntfilnam, '/', back=.true.), index(pntfilnam, '\', back=.true.))
      pointname = pntfilnam(indstart + 1:)

      indend = index(pointname, ' ')
      if (indend > 0) then
         pointname(indend:) = ' '
      end if

      indend = index(pointname, '.')
      if (indend > 0) then
         pointname(indend:) = ' '
      end if

      if (len_trim(pointname) > maxPointNameLength) then
         write (*, '(5a)') "*** MESSAGE: point name '", trim(pointname), "' is truncated to '", pointname(:maxPointNameLength), "'."
         pointname(maxPointNameLength + 1:) = ' '
      end if
   end function get_pointname

!> add nest index as to the string
   subroutine add_inest(nnest, line, i, inest)

      implicit none

      integer, intent(in) :: nnest
      integer, intent(in) :: inest
      integer, intent(inout) :: i
      character(256), intent(inout) :: line

      if (nnest > 1) then
         line(i:) = 'n'
         write (line(i + 1:), '(I2.2)') inest
         i = i + 3
      end if
   end subroutine

end module swan_input
