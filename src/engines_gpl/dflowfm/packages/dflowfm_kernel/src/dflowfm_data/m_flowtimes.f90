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
   use precision, only: dp
   implicit none

   character(len=8) :: refdat !< Reference date (e.g., '20090101'). All times (tstart_user, tend_user, etc.) are w.r.t. to this date.
   integer :: julrefdat !< will be set by calling settimespacerefdat
   real(kind=dp) :: refdate_mjd !< Reference date as modified Julian date
   integer :: irefdate !< Reference date (e.g., 20090101)
   real(kind=dp) :: Tzone !< Data Sources in GMT are interrogated with time in minutes since refdat-Tzone*60
   character(len=42) :: Tudunitstr !< Complete UDunitstring for the time variable written as a unit attribute into various NetCDF output files
   integer, parameter :: tunit = 1 !< Times to EC-module are in seconds
   real(kind=dp) :: Timjan !< time in hours of refdat relative to Januari 1 of the same year
   real(kind=dp) :: dt_user !< User specified time step (s) for external forcing update.
   real(kind=dp) :: dt_nodal !< User specified time step (s) for nodal factors update.
   real(kind=dp) :: dt_max !< Computational timestep limit by user.
   real(kind=dp) :: dt_init !< dt of first timestep, if not specified, use dt_max, if that also not specified, use 1 s

   integer :: ja_timestep_auto !< Use CFL-based dt (with dt_max as upper bound)
   integer :: ja_timestep_auto_visc !< Use explicit time step restriction based on viscosity term
   integer :: ja_timestep_nostruct !< Exclude (structure) links without advection from the time step limitation
   integer :: ja_timestep_noqout !< Exclude negative qin term from timestep limitation.
   real(kind=dp) :: tstart_user !< User specified time start (s) w.r.t. refdat
   real(kind=dp) :: tstart_tlfsmo_user !< User specified start time of tlfsmo (s) w.r.t. refdat
   real(kind=dp) :: tstop_user !< User specified time stop  (s) w.r.t. refdat
   real(kind=dp) :: time_user !< Next time of external forcings update (steps increment by dt_user).

   real(kind=dp) :: dts !< internal computational timestep (s)
   real(kind=dp) :: dtsc !< max timstep of limiting point kkcflmx, zero if larger than dt_max
   real(kind=dp) :: dt_fac_max !< max dts increase factor
   real(kind=dp) :: dti !< inverse  computational timestep (1/s)
   real(kind=dp) :: dtprev !< previous computational timestep (s)  (1s is a bit like sobek)
   real(kind=dp) :: dtmin !< dt < dtmin : surely crashed
   real(kind=dp) :: dtminbreak !< smallest allowed timestep (in s), checked on a sliding average of several timesteps in validation routine.
   real(kind=dp) :: dtminhis !< smallest timestep within most recent his interval
   real(kind=dp) :: tfac !< time unit in seconds
   real(kind=dp), allocatable :: tvalswindow(:) !< (NUMDTWINDOWSIZE) Time1 values in a moving time window to compute sliding average dt
   integer, parameter :: NUMDTWINDOWSIZE = 100 !< Number of time steps to include in the sliding average, don't set this too optimistic to avoid too fast simulation breaks.
   integer :: idtwindow_start !< Current start index in tvalswindow(:) array. This array is filled in a cyclic order, with never more than NUMDTWINDOWSIZE time values.
   real(kind=dp) :: time0 !< current   julian (s) of s0
   real(kind=dp) :: time1 !< current   julian (s) of s1  ! and of course, time1 = time0 + dt
   real(kind=dp) :: tim1bnd !< last time boundary signals were given
   real(kind=dp) :: tim1fld !< last time field    signals were given
   integer :: ja_time_step_analysis = 0
   real(kind=dp), allocatable :: dtcell(:) !< time step per cell based on CFL (s), size:ndkx
   real(kind=dp), allocatable :: time_wetground(:) !< Cumulative time when water is above ground level, size: ndxi (now only for 1d, later also for 2d)

   !TODO: use in the trachytopes this variable and fully remove reading from rdtrt
   real(kind=dp) :: dt_trach !< DtTrt Trachytope roughness update time interval (s)

   real(kind=dp) :: dnt_user !< counter for nr of user steps    ( )
   real(kind=dp) :: dnt !< number of timesteps ( )
   real(kind=dp) :: dnums1it !< total nr of non-linear continuity iterations

   real(kind=dp) :: fhr !< Factor sec hrs
   real(kind=dp) :: fday !< Factor sec day

   real(kind=dp) :: ti_map !< map interval (s)
   real(kind=dp) :: ti_maps !< Start of map output period (as assigned in mdu-file) (s)
   real(kind=dp) :: ti_mape !< End   of map output period (as assigned in mdu-file) (s)
   real(kind=dp) :: ti_his !< history interval (s)
   real(kind=dp) :: ti_hiss !< Start of his output period (as assigned in mdu-file) (s)
   real(kind=dp) :: ti_hise !< End   of his output period (as assigned in mdu-file) (s)
   real(kind=dp) :: ti_wav !< averaging interval spatial wave quantities (s)
   real(kind=dp) :: ti_wavs !< averaging interval spatial wave quantities
   real(kind=dp) :: ti_wave !< averaging interval spatial wave quantities
   real(kind=dp) :: ti_com !< com file interval (s)
   real(kind=dp) :: ti_coms !< Start of com file output period (as assigned in mdu-file) (s)
   real(kind=dp) :: ti_come !< End   of com file output period (as assigned in mdu-file) (s)
   real(kind=dp) :: ti_sed !< averaging interval sedmor quantities (s)
   real(kind=dp) :: ti_seds !< averaging interval sedmor quantities
   real(kind=dp) :: ti_sede !< averaging interval sedmor quantities
   real(kind=dp) :: ti_st !< averaging interval sedtrails quantities (s)
   real(kind=dp) :: ti_sts !< averaging interval sedtrails wave quantities
   real(kind=dp) :: ti_ste !< averaging interval sedtrails wave quantities
   real(kind=dp) :: ti_xls !< history interval (s) xls
   real(kind=dp) :: ti_rst !< restart interval (s)
   real(kind=dp) :: ti_rsts !< Start of restart output period (as assigned in mdu-file) (s)
   real(kind=dp) :: ti_rste !< End   of restart output period (as assigned in mdu-file) (s)
   real(kind=dp) :: ti_mba !< Time step for mass balance area output
   real(kind=dp) :: ti_waq !< Interval between output in delwaq files (s).
   real(kind=dp) :: ti_waqs !< Start of WAQ output period
   real(kind=dp) :: ti_waqe !< End   of WAQ output period
   logical :: wrwaqon = .false. !< Waq output was initialised
   real(kind=dp) :: ti_waqproc !< Time step for water quality processes

   real(kind=dp) :: ti_classmap !< class map interval (s)
   real(kind=dp) :: ti_classmaps !< Start of class map output period (as assigned in mdu-file) (s)
   real(kind=dp) :: ti_classmape !< End   of class map output period (as assigned in mdu-file) (s)
   real(kind=dp), allocatable, target :: map_classes_s1(:) !< classes for water level
   real(kind=dp), allocatable, target :: map_classes_hs(:) !< classes for water depth
   real(kind=dp), allocatable, target :: map_classes_ucmag(:) !< classes for the magnitude of the velocity
   real(kind=dp), allocatable, target :: map_classes_ucdir(:) !< classes for the direction of the velocity
   real(kind=dp) :: map_classes_ucdirstep !< step size of classes for the direction of the velocity
   real(kind=dp) :: ti_stat !< Interval between simulation statistics output (s).
   real(kind=dp) :: ti_timings !< (parallel) timings output interval
   real(kind=dp) :: ti_split !< Time interval for time splitting: time after which new his/map file will be created (e.g. montly), see also the unit below.
                                                  !! Default is 0 to have no time-splitting of output files.
   character(len=1) :: ti_split_unit !< Unit for time splitting interval: Y: years, M: months, D: days, h:hours, m: minutes, s: seconds.
   real(kind=dp), allocatable :: ti_mpt(:) !< times for writing map-files (s), possibly non-equidistant in time
   real(kind=dp), allocatable :: ti_mpt_rel(:) !< times for writing map-files (s) relative to current time, possibly non-equidistant in time
   real(kind=dp), allocatable :: ti_ctv(:) !< times for writing com-files (s), possibly non-equidistant in time
   real(kind=dp), allocatable :: ti_ctv_rel(:) !< times for writing com-files (s) relative to current time, possibly non-equidistant in time
   real(kind=dp) :: tmini !< Initial time for updating map/his/rst

   real(kind=dp) :: time_choice !< Time consisting the next time_user / time_map
   real(kind=dp) :: time_out !< Next time for output in the most general sense (map, his, etc.)
   real(kind=dp) :: time_map !< Map output interval
   real(kind=dp) :: time_com !< Com output interval
   real(kind=dp) :: time_wav !< Time-avg'd output interval xb JRE
   real(kind=dp) :: time_sed !< Time-avg'd output interval sedmor
   real(kind=dp) :: time_st !< Time-avg'd output interval sedtrails
   real(kind=dp) :: time_his !< Next time for his output
   real(kind=dp) :: time_xls !< Next time for his output
   real(kind=dp) :: time_rst !< Next time for restart output
   real(kind=dp) :: time_classmap !< Next time for class map output
   real(kind=dp) :: time_waq !< Next time for delwaq output
   real(kind=dp) :: time_waqset !< Next time to reset the quantitis for waq
   real(kind=dp) :: time_waqproc !< Next time to calcualate waq processes
   real(kind=dp) :: time_mba !< Next time to process mass balances
   real(kind=dp) :: time_stat !< Next time for simulation statistics output
   real(kind=dp) :: time_timings !< Next time for timings output
   real(kind=dp) :: time_split !< Next time for a new time-split output file.
   real(kind=dp) :: time_split0 !< Start time for the current time-split output file.
   real(kind=dp) :: time_fetch !< next time fetchlength will be established
   real(kind=dp) :: tifetch = 0 !< fetchlength comp. interval

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

   real(kind=dp) :: dsetb !< number of setbacks ()
   real(kind=dp) :: walltime0 !< wall time at start of timeloop (s)

   character(len=20) :: rundat0 !< start and end date (wallclock) of computer run
   character(len=20) :: rundat2 !< start and end date (wallclock) of computer run format = _yymmddhhmmss
   character(len=20) :: restart_date_time = ' ' !< desired time to be taken from restart map files
   character(len=14) :: start_date_time = ' ' !< optional replacement of Tstart_user
   character(len=14) :: start_date_time_tlfsmo = ' ' !< optional replacement of TstartTlfsmo_user
   character(len=14) :: stop_date_time = ' ' !< optional replacement of Tstop_user
   integer :: jarestart !< use restart yes/no, 1/0

   real(kind=dp) :: tlfsmo = 0.0_dp !< fourier bnd smoothing times
   real(kind=dp) :: alfsmo = 1.0_dp !< fourier bnd smoothing weight factor
   integer :: keepstbndonoutflow = 1 !< keep them on outflow = 1

   real(kind=dp) :: t_spinup_turb_log_prof = 0.0_dp !< From Tstart to Tstart+t_spinup_turb_log_prof, Turbulent profiles based on log profiles (<= 0: No)
   real(kind=dp) :: alfaspin
   real(kind=dp) :: dt_update_roughness !< Update interval for time dependent roughness values (from frictFile).
   real(kind=dp) :: times_update_roughness(2) !< Time window for wich the current time dependent roughness values (from FrictFile) are valid.

contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, call reset_flowtimes() instead.
   subroutine default_flowtimes()
      refdat = '20010101' !< Reference date (e.g., '20090101'). All times (tstart_user, tend_user, etc.) are w.r.t. to this date.
      irefdate = 20010101
      Tzone = 0.0_dp
      dt_user = 120.0_dp !< User specified time step (s) for external forcing update.
      dt_nodal = 21600.0_dp !< User specified time step (s) for nodal factors update.
      dt_max = 30.0_dp !< Computational timestep limit by user.
      dtmin = 1d-4 !< dt < dtmin : surely crashed
      dtminbreak = 0.0_dp !< smallest allowed timestep, otherwise break: off
      dtminhis = 9d9 !< smallest timestep within most recent his interval
      dt_init = 1.0_dp
      dt_trach = 1200.0_dp !< User specified DtTrt Trachytope roughness update time interval (s)
      dt_fac_max = 1.1_dp !< default setting
      ja_timestep_auto = 1 !< Use CFL-based dt (with dt_max as upper bound)
      ja_timestep_auto_visc = 0 !< Use explicit time step restriction based on viscosity term
      ja_timestep_nostruct = 0 !< Exclude (structure) links without advection from the time step limitation
      ja_timestep_noqout = 1 !< Exclude negative qin terms from the time step limitation
      tstart_user = 0.0_dp !< User specified time start (s) w.r.t. refdat
      tstop_user = 100 * 24 * 3600 !< User specified time stop  (s) w.r.t. refdat
      time_user = tstart_user !< Next time of external forcings update (steps increment by dt_user).
      tstart_tlfsmo_user = tstart_user !< Start time of tlfsmo (s) w.r.t. refdat

      dnt_user = 0 !< counter for nr of user steps    ( )
      dnt = 0 !< number of timesteps ( )

      fhr = 1.0_dp / 3600.0_dp !< Factor sec hrs
      fday = 1.0_dp / (3600.0_dp * 24.0_dp) !< Factor sec day

      ti_wav = 1200.0_dp !< wave avg'ing interval (s), 20 minutes okay default  JRE
      ti_wavs = 0.0_dp
      ti_wave = 0.0_dp
      ti_map = 1200.0_dp !< map interval (s)
      ti_maps = 0.0_dp !< Start map output (s)
      ti_mape = 0.0_dp !< End   map output (s)
      ti_his = 120.0_dp !< history interval (s)
      ti_hiss = 0.0_dp !< Start history output (s)
      ti_hise = 0.0_dp !< End   history output (s)
      ti_com = dt_user !< com interval (s)
      ti_coms = 0.0_dp !< Start com output (s)
      ti_come = 0.0_dp !< End com output (s)
      ti_sed = 0.0_dp !< Time-avg'd output interval sedmor (s), (Default: off)
      ti_seds = 0.0_dp !< Start time-avg'd output sedmor (s)
      ti_sede = 0.0_dp !< End   time-avg'd output sedmor (s)
      ti_st = 3600.0_dp !< Time-avg'd output interval sedtrails
      ti_sts = 0.0_dp !< Start time-avg'd output sedtrails
      ti_ste = 0.0_dp !< End   time-avg'd output sedtrails
      ti_xls = 0.0_dp !< history interval (s) xls, (Default: off)
      ti_rst = 24.0_dp * 3600.0_dp !< Restart interval (s)
      ti_rsts = 0.0_dp !< Start restart output (s)
      ti_rste = 0.0_dp !< End   restart output (s)
      ti_mba = 0.0_dp
      ti_waq = 0.0_dp !< delwaq interval (s) (Default: off)
      ti_waqproc = 0.0_dp
      ti_stat = -60.0_dp !< simulation statistics interval (s) (Default: off, will later default to dt_user), <0: use wc-time
      ti_timings = 0.0_dp !< timings output interval
      ti_split = 0.0_dp !< Time interval for time splitting of output files.
      ti_split_unit = 's' !< Unit for time partitioning interval

      ti_classmap = 0.0_dp !< Class map interval (s), (Default: off)
      ti_classmaps = 0.0_dp !< Start class map output (s)
      ti_classmape = 0.0_dp !< End   class map output (s)
      map_classes_ucdirstep = -999.0_dp !< default no step size given for classes of flow direction
      if (allocated(map_classes_ucdir)) deallocate (map_classes_ucdir)

      tmini = -1d9 !< initial time for updating the 4 above

      dt_update_roughness = 86400.0_dp

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
      dti = 1.0_dp / dts !< inverse  computational timestep (1/s)
      time0 = 0.0_dp !< current   julian (s) of s0
      time1 = 0.0_dp !< current   julian (s) of s1  ! and of course, time1 = time0 + dt
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
      if (ti_stat > 0.0_dp) then
         time_stat = tstart_user !< next model time for simulation statistics output
      else
         time_stat = 0.0_dp !< next wall-clock time for simulation statistics output
      end if
      time_timings = tstart_user !< next time for timing output
      time_split = tstart_user !< next time for a new time-split output file
      time_split0 = time_split !< Start time for the current time-split output file.
      if (dtminbreak > 0.0_dp) then
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
      if (tlfsmo > 0.0_dp) then !  Smoothing period
         alfsmo = 0.0_dp !  Smoothing factor
      else
         alfsmo = 1.0_dp
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
