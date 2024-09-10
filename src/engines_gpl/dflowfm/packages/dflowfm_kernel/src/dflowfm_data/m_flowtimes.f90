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

 !> this module contains the real flow times, only to be managed by setting times in module m_usertimes
module m_flowtimes
   implicit none

   character(len=8) :: refdat !< Reference date (e.g., '20090101'). All times (tstart_user, tend_user, etc.) are w.r.t. to this date.
   integer :: julrefdat !< will be set by calling settimespacerefdat
   double precision :: refdate_mjd !< Reference date as modified Julian date
   integer :: irefdate !< Reference date (e.g., 20090101)
   double precision :: Tzone !< Data Sources in GMT are interrogated with time in minutes since refdat-Tzone*60
   character(len=42) :: Tudunitstr !< Complete UDunitstring for the time variable written as a unit attribute into various NetCDF output files
   integer, parameter :: tunit = 1 !< Times to EC-module are in seconds
   double precision :: Timjan !< time in hours of refdat relative to Januari 1 of the same year
   double precision :: dt_user !< User specified time step (s) for external forcing update.
   double precision :: dt_nodal !< User specified time step (s) for nodal factors update.
   double precision :: dt_max !< Computational timestep limit by user.
   double precision :: dt_init !< dt of first timestep, if not specified, use dt_max, if that also not specified, use 1 s

   integer :: ja_timestep_auto !< Use CFL-based dt (with dt_max as upper bound)
   integer :: ja_timestep_auto_visc !< Use explicit time step restriction based on viscosity term
   integer :: ja_timestep_nostruct !< Exclude (structure) links without advection from the time step limitation
   integer :: ja_timestep_noqout !< Exclude negative qin term from timestep limitation.
   double precision :: tstart_user !< User specified time start (s) w.r.t. refdat
   double precision :: tstart_tlfsmo_user !< User specified start time of tlfsmo (s) w.r.t. refdat
   double precision :: tstop_user !< User specified time stop  (s) w.r.t. refdat
   double precision :: time_user !< Next time of external forcings update (steps increment by dt_user).

   double precision :: dts !< internal computational timestep (s)
   double precision :: dtsc !< max timstep of limiting point kkcflmx, zero if larger than dt_max
   double precision :: dtfacmax !< max dts increase factor
   double precision :: dti !< inverse  computational timestep (1/s)
   double precision :: dtprev !< previous computational timestep (s)  (1s is a bit like sobek)
   double precision :: dtmin !< dt < dtmin : surely crashed
   double precision :: dtminbreak !< smallest allowed timestep (in s), checked on a sliding average of several timesteps in validation routine.
   double precision :: dtminhis !< smallest timestep within most recent his interval
   double precision :: tfac !< time unit in seconds
   double precision, allocatable :: tvalswindow(:) !< (NUMDTWINDOWSIZE) Time1 values in a moving time window to compute sliding average dt
   integer, parameter :: NUMDTWINDOWSIZE = 100 !< Number of time steps to include in the sliding average, don't set this too optimistic to avoid too fast simulation breaks.
   integer :: idtwindow_start !< Current start index in tvalswindow(:) array. This array is filled in a cyclic order, with never more than NUMDTWINDOWSIZE time values.
   double precision :: time0 !< current   julian (s) of s0
   double precision :: time1 !< current   julian (s) of s1  ! and of course, time1 = time0 + dt
   double precision :: tim1bnd !< last time boundary signals were given
   double precision :: tim1fld !< last time field    signals were given
   integer :: jatimestepanalysis = 0
   double precision, allocatable :: dtcell(:) !< time step per cell based on CFL (s), size:ndkx
   double precision, allocatable :: time_wetground(:) !< Cumulative time when water is above ground level, size: ndxi (now only for 1d, later also for 2d)

   !TODO: use in the trachytopes this variable and fully remove reading from rdtrt
   double precision :: dt_trach !< DtTrt Trachytope roughness update time interval (s)

   double precision :: dnt_user !< counter for nr of user steps    ( )
   double precision :: dnt !< number of timesteps ( )
   double precision :: dnums1it !< total nr of non-linear continuity iterations

   double precision :: fhr !< Factor sec hrs
   double precision :: fday !< Factor sec day

   double precision :: ti_map !< map interval (s)
   double precision :: ti_maps !< Start of map output period (as assigned in mdu-file) (s)
   double precision :: ti_mape !< End   of map output period (as assigned in mdu-file) (s)
   double precision :: ti_his !< history interval (s)
   double precision :: ti_hiss !< Start of his output period (as assigned in mdu-file) (s)
   double precision :: ti_hise !< End   of his output period (as assigned in mdu-file) (s)
   double precision :: ti_wav !< averaging interval spatial wave quantities (s)
   double precision :: ti_wavs !< averaging interval spatial wave quantities
   double precision :: ti_wave !< averaging interval spatial wave quantities
   double precision :: ti_com !< com file interval (s)
   double precision :: ti_coms !< Start of com file output period (as assigned in mdu-file) (s)
   double precision :: ti_come !< End   of com file output period (as assigned in mdu-file) (s)
   double precision :: ti_sed !< averaging interval sedmor quantities (s)
   double precision :: ti_seds !< averaging interval sedmor quantities
   double precision :: ti_sede !< averaging interval sedmor quantities
   double precision :: ti_st !< averaging interval sedtrails quantities (s)
   double precision :: ti_sts !< averaging interval sedtrails wave quantities
   double precision :: ti_ste !< averaging interval sedtrails wave quantities
   double precision :: ti_xls !< history interval (s) xls
   double precision :: ti_rst !< restart interval (s)
   double precision :: ti_rsts !< Start of restart output period (as assigned in mdu-file) (s)
   double precision :: ti_rste !< End   of restart output period (as assigned in mdu-file) (s)
   double precision :: ti_mba !< Time step for mass balance area output
   double precision :: ti_waq !< Interval between output in delwaq files (s).
   double precision :: ti_waqs !< Start of WAQ output period
   double precision :: ti_waqe !< End   of WAQ output period
   logical :: wrwaqon = .false. !< Waq output was initialised
   double precision :: ti_waqproc !< Time step for water quality processes

   double precision :: ti_classmap !< class map interval (s)
   double precision :: ti_classmaps !< Start of class map output period (as assigned in mdu-file) (s)
   double precision :: ti_classmape !< End   of class map output period (as assigned in mdu-file) (s)
   double precision, allocatable, target :: map_classes_s1(:) !< classes for water level
   double precision, allocatable, target :: map_classes_hs(:) !< classes for water depth
   double precision, allocatable, target :: map_classes_ucmag(:) !< classes for the magnitude of the velocity
   double precision, allocatable, target :: map_classes_ucdir(:) !< classes for the direction of the velocity
   double precision :: map_classes_ucdirstep !< step size of classes for the direction of the velocity
   double precision :: ti_stat !< Interval between simulation statistics output (s).
   double precision :: ti_timings !< (parallel) timings output interval
   double precision :: ti_split !< Time interval for time splitting: time after which new his/map file will be created (e.g. montly), see also the unit below.
                                                  !! Default is 0 to have no time-splitting of output files.
   character(len=1) :: ti_split_unit !< Unit for time splitting interval: Y: years, M: months, D: days, h:hours, m: minutes, s: seconds.
   double precision, allocatable :: ti_mpt(:) !< times for writing map-files (s), possibly non-equidistant in time
   double precision, allocatable :: ti_mpt_rel(:) !< times for writing map-files (s) relative to current time, possibly non-equidistant in time
   double precision, allocatable :: ti_ctv(:) !< times for writing com-files (s), possibly non-equidistant in time
   double precision, allocatable :: ti_ctv_rel(:) !< times for writing com-files (s) relative to current time, possibly non-equidistant in time
   double precision :: tmini !< Initial time for updating map/his/rst

   double precision :: time_choice !< Time consisting the next time_user / time_map
   double precision :: time_out !< Next time for output in the most general sense (map, his, etc.)
   double precision :: time_map !< Map output interval
   double precision :: time_com !< Com output interval
   double precision :: time_wav !< Time-avg'd output interval xb JRE
   double precision :: time_sed !< Time-avg'd output interval sedmor
   double precision :: time_st !< Time-avg'd output interval sedtrails
   double precision :: time_his !< Next time for his output
   double precision :: time_xls !< Next time for his output
   double precision :: time_rst !< Next time for restart output
   double precision :: time_classmap !< Next time for class map output
   double precision :: time_waq !< Next time for delwaq output
   double precision :: time_waqset !< Next time to reset the quantitis for waq
   double precision :: time_waqproc !< Next time to calcualate waq processes
   double precision :: time_mba !< Next time to process mass balances
   double precision :: time_stat !< Next time for simulation statistics output
   double precision :: time_timings !< Next time for timings output
   double precision :: time_split !< Next time for a new time-split output file.
   double precision :: time_split0 !< Start time for the current time-split output file.
   double precision :: time_fetch !< next time fetchlength will be established
   double precision :: tifetch = 0 !< fetchlength comp. interval

   integer :: it_map !< Nr of snapshots presently in map file
   integer :: it_wav !< Nr of snapshots presently in time-avg'd wave output file JRE
   integer :: it_sed !< Nr of snapshots presently in time-avg'd sedmor output file JRE
   integer :: it_map_tec !< Nr of snapshots presently in map file, Tecplot format
   integer :: it_his !< Nr of snapshots presently in his file
   integer :: it_inc !< Nr of lines     presently in inc file
   integer :: it_rst !< Nr of snapshots presently in rst file
   integer :: it_waq !< Nr of snapshots presently in delwaq files.
   integer :: it_stat !< Nr of simulation statistics presently in log file.
   integer :: it_st !< Nr of simulation statistics presently in sedtrails output file.
   ! for performance timings
   logical :: debugtimeon !< timing yes or no
   integer :: handle_user !< timer handle for user timesteps
   integer :: handle_steps !< timer handle for timesteps
   integer :: handle_umod !< timer handle for set-umod
   integer :: handle_sol !< timer handle for conj-grad
   integer :: handle_furu !< timer handle for furu
   integer :: handle_all !< timer handle for steps + plots
   integer :: handle_inistep !< timer handle for inistep
   integer :: handle_iniext !< timer handle for init externalforcings
   integer :: handle_ext !< timer handle for externalforcings
   integer :: handle_extbnd !< timer handle for externalforcingsonbnd
   integer :: handle_fetch !< timer handle for externalforcings fetch model
   integer :: handle_extra(90) !< timer handles for extra timers

   double precision :: dsetb !< number of setbacks ()
   double precision :: walltime0 !< wall time at start of timeloop (s)

   character(len=20) :: rundat0 !< start and end date (wallclock) of computer run
   character(len=20) :: rundat2 !< start and end date (wallclock) of computer run format = _yymmddhhmmss
   character(len=20) :: restartdatetime = ' ' !< desired time to be taken from restart map files
   character(len=14) :: Startdatetime = ' ' !< optional replacement of Tstart_user
   character(len=14) :: Stopdatetime = ' ' !< optional replacement of Tstop_user
   integer :: jarestart !< use restart yes/no, 1/0

   double precision :: tlfsmo = 0d0 !< fourier bnd smoothing times
   double precision :: alfsmo = 1d0 !< fourier bnd smoothing weight factor
   integer :: keepstbndonoutflow = 0 !< keep them on outflow = 1

   double precision :: Tspinupturblogprof = 0d0 !< From Tstart to Tstart+Tspinupturblogprof, Turbulent profiles based on log profiles
   !< 0d0 = No
   double precision :: alfaspin
   double precision :: dt_UpdateRoughness !< Update interval for time dependent roughness values (from frictFile).
   double precision :: times_update_roughness(2) !< Time window for wich the current time dependent roughness values (from FrictFile) are valid.

contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, call reset_flowtimes() instead.
   subroutine default_flowtimes()
      refdat = '20010101' !< Reference date (e.g., '20090101'). All times (tstart_user, tend_user, etc.) are w.r.t. to this date.
      irefdate = 20010101
      Tzone = 0d0
      dt_user = 120d0 !< User specified time step (s) for external forcing update.
      dt_nodal = 21600d0 !< User specified time step (s) for nodal factors update.
      dt_max = 30d0 !< Computational timestep limit by user.
      dtmin = 1d-4 !< dt < dtmin : surely crashed
      dtminbreak = 0d0 !< smallest allowed timestep, otherwise break: off
      dtminhis = 9d9 !< smallest timestep within most recent his interval
      dt_init = 1d0
      dt_trach = 1200d0 !< User specified DtTrt Trachytope roughness update time interval (s)
      dtfacmax = 1.1d0 !< default setting
      ja_timestep_auto = 1 !< Use CFL-based dt (with dt_max as upper bound)
      ja_timestep_auto_visc = 0 !< Use explicit time step restriction based on viscosity term
      ja_timestep_nostruct = 0 !< Exclude (structure) links without advection from the time step limitation
      ja_timestep_noqout = 1 !< Exclude negative qin terms from the time step limitation
      tstart_user = 0d0 !< User specified time start (s) w.r.t. refdat
      tstop_user = 100 * 24 * 3600 !< User specified time stop  (s) w.r.t. refdat
      time_user = tstart_user !< Next time of external forcings update (steps increment by dt_user).
      tstart_tlfsmo_user = tstart_user !< Start time of tlfsmo (s) w.r.t. refdat

      dnt_user = 0 !< counter for nr of user steps    ( )
      dnt = 0 !< number of timesteps ( )

      fhr = 1d0 / 3600d0 !< Factor sec hrs
      fday = 1d0 / (3600d0 * 24d0) !< Factor sec day

      ti_wav = 1200d0 !< wave avg'ing interval (s), 20 minutes okay default  JRE
      ti_wavs = 0d0
      ti_wave = 0d0
      ti_map = 1200d0 !< map interval (s)
      ti_maps = 0d0 !< Start map output (s)
      ti_mape = 0d0 !< End   map output (s)
      ti_his = 120d0 !< history interval (s)
      ti_hiss = 0d0 !< Start history output (s)
      ti_hise = 0d0 !< End   history output (s)
      ti_com = dt_user !< com interval (s)
      ti_coms = 0d0 !< Start com output (s)
      ti_come = 0d0 !< End com output (s)
      ti_sed = 0d0 !< Time-avg'd output interval sedmor (s), (Default: off)
      ti_seds = 0d0 !< Start time-avg'd output sedmor (s)
      ti_sede = 0d0 !< End   time-avg'd output sedmor (s)
      ti_st = 3600d0 !< Time-avg'd output interval sedtrails
      ti_sts = 0d0 !< Start time-avg'd output sedtrails
      ti_ste = 0d0 !< End   time-avg'd output sedtrails
      ti_xls = 0d0 !< history interval (s) xls, (Default: off)
      ti_rst = 24d0 * 3600d0 !< Restart interval (s)
      ti_rsts = 0d0 !< Start restart output (s)
      ti_rste = 0d0 !< End   restart output (s)
      ti_mba = 0d0
      ti_waq = 0d0 !< delwaq interval (s) (Default: off)
      ti_waqproc = 0d0
      ti_stat = -60d0 !< simulation statistics interval (s) (Default: off, will later default to dt_user), <0: use wc-time
      ti_timings = 0d0 !< timings output interval
      ti_split = 0d0 !< Time interval for time splitting of output files.
      ti_split_unit = 's' !< Unit for time partitioning interval

      ti_classmap = 0d0 !< Class map interval (s), (Default: off)
      ti_classmaps = 0d0 !< Start class map output (s)
      ti_classmape = 0d0 !< End   class map output (s)
      map_classes_ucdirstep = -999d0 !< default no step size given for classes of flow direction
      if (allocated(map_classes_ucdir)) deallocate (map_classes_ucdir)

      tmini = -1d9 !< initial time for updating the 4 above

      dt_UpdateRoughness = 86400d0

      ! Wall clock timers are restarted here already, because some timers are started *prior* to flow_modelinit().
      call reset_timers()

      ! Remaining of variables is handled in reset_flowtimes()
      call reset_flowtimes()
   end subroutine default_flowtimes

!> Resets only flow times variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, use default_flowtimes() instead.
   subroutine reset_flowtimes()
      use Timers
      dtprev = dt_init !< previous computational timestep (s)  (1s is a bit like sobek)
      dts = dt_init !< internal computational timestep (s)
      dti = 1d0 / dts !< inverse  computational timestep (1/s)
      time0 = 0d0 !< current   julian (s) of s0
      time1 = 0d0 !< current   julian (s) of s1  ! and of course, time1 = time0 + dt
      tim1bnd = -9d9 !< last time bnd signals were given
      tim1fld = -9d9 !< last time bnd signals were given

      call setTUDUnitString()

      time_user = tstart_user !< lijkt me ook onderdeel
      time_map = tstart_user !< next time for map output
      time_wav = tstart_user !< same, wav
      time_sed = tstart_user !< same, morstats
      time_st = tstart_user !< same, sedtrails
      time_his = tstart_user !< next time for his output
      time_xls = tstart_user !< next time for his output
      time_rst = tstart_user !< next time for restart output
      time_classmap = tstart_user !< next time for class map output
      time_fetch = tstart_user !< next time for fetch establ.

      time_waq = ti_waqs !< next time for waq output, starting at the output start time
      time_waqset = tstart_user !< next time for reset the quantities for waq output
      time_waqproc = tstart_user + ti_waqproc !< next time for wq processes
      time_mba = tstart_user + ti_mba !< next time for balance update
      if (ti_stat > 0d0) then
         time_stat = tstart_user !< next model time for simulation statistics output
      else
         time_stat = 0d0 !< next wall-clock time for simulation statistics output
      end if
      time_timings = tstart_user !< next time for timing output
      time_split = tstart_user !< next time for a new time-split output file
      time_split0 = time_split !< Start time for the current time-split output file.
      if (dtminbreak > 0d0) then
         if (.not. allocated(tvalswindow)) then
            allocate (tvalswindow(NUMDTWINDOWSIZE))
         end if
         idtwindow_start = 1 ! Start fresh, with first time0 on pos #1.
         tvalswindow(idtwindow_start) = tstart_user
      end if

      it_map = 0 !< Nr of snapshots presently in map file
      it_wav = 0 !< Nr of snapshots presently in time-avg'd file JRE
      it_sed = 0 !< Nr of snapshots presently in time-avg'd sed file JRE
      it_st = 0 !< Nr of snapshots presently in time-avg'd sedtrails file JRE
      it_map_tec = 0 !< Nr of snapshots presently in map file
      it_his = 0 !< Nr of snapshots presently in his file
      it_inc = 0 !< Nr of lines     presently in inc file
      it_rst = 0 !< Nr of snapshots presently in rst file
      it_waq = 0 !< Nr of snapshots presently in waq couple files
      it_stat = 0 !< Nr of simulation statistics presently in log file.

      times_update_roughness(1:2) = tstart_user

! for performance timings
      debugtimeon = .false. !< timing yes or no

      dsetb = 0 !< number of setbacks ()
      if (tlfsmo > 0d0) then !  Smoothing period
         alfsmo = 0d0 !  Smoothing factor
      else
         alfsmo = 1d0
      end if

   end subroutine reset_flowtimes

   subroutine reset_timers()
      use Timers

      call timini()
      timon = .true.

      handle_user = 0
      handle_steps = 0
      handle_umod = 0
      handle_sol = 0
      handle_furu = 0
      handle_all = 0
      handle_inistep = 0
      handle_iniext = 0
      handle_ext = 0
      handle_extbnd = 0
      handle_fetch = 0
      handle_extra = 0

      call timstrt('All', handle_all)
   end subroutine reset_timers

!> Sets the UDUnit timestring based on current model time settings.
!! Module variable Tudunitstr can the be used in various output routines.
   subroutine setTUDUnitString()
      integer :: Tzonehrs
      character(len=1) :: Tzonesgn

      Tzonehrs = int(TZone)
      if (Tzone < 0) then
         Tzonesgn = '-'
      else
         Tzonesgn = '+'
      end if
      write (Tudunitstr, '(a,i2.2,a)') 'seconds since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00 '//Tzonesgn, abs(Tzonehrs), ':00'

   end subroutine setTUDUnitString

end module m_flowtimes
