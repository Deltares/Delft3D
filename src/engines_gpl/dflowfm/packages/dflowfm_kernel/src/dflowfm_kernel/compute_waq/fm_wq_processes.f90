!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2018-2024.
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

submodule(m_fm_wq_processes_sub) m_fm_wq_processes_sub_

   implicit none

contains

   module subroutine fm_wq_processes_ini_sub()
      use m_add_bndtracer, only: add_bndtracer
      use m_getkbotktopmax
      use m_fm_wq_processes
      use m_alloc
      use m_flow, only: kmx
      use m_flowgeom, only: Ndxi
      use fm_external_forcings_data
      use m_transport
      use m_partitioninfo
      use unstruc_model
      use m_flowparameters, only: jawriteDetailedTimers
      use unstruc_files, only: mdia
      use m_flowtimes
      use timers
      use m_string_utils, only: index_in_array
      use m_logger_helper, only: set_log_unit_number
      use system_utils, only: get_executable_directory

      integer :: ierr_sub !< error status
      integer :: ierr_eho !< error status

      character(256) :: cerr !< error message
      character(len=1024) :: exe_dir, share_dir
      ! Other
      integer(4) :: nosys_eho, notot_eho, nocons_eho
      integer(4) :: i

      integer :: janew, iex, ierr
      integer :: kk, k, kb, kt, ktmax, kdum

      logical :: Lsub, Leho, Lstt, Lpdf, Lopl, Lblm, Lallocated

      integer(4), save :: ithndl = 0

      call mess(LEVEL_INFO, 'Initialising water quality processes')

      jawriteDetailedTimers = 1
      if (timon) call timstrt("fm_wq_processes_ini_sub", ithndl)

      ibflag = 0

      substance_file = md_subfile
      his_output_file = md_ehofile
      proc_def_file = md_pdffile
      proc_dllso_file = md_oplfile
      bloom_file = md_blmfile
      statistics_file = md_sttfile

      ! Get executable directory
      call get_executable_directory(exe_dir, ierr) 
      share_dir = trim(exe_dir)//'../share/delft3d/'

      ! check if substance file exists
      inquire (file=substance_file, exist=Lsub)
      if (.not. Lsub) then
         call mess(LEVEL_ERROR, 'Substance file does not exist: ', trim(substance_file))
      end if

      ! check if additional history output file exists
      if (his_output_file /= ' ') then
         inquire (file=his_output_file, exist=Leho)
         if (.not. Leho) then
            call mess(LEVEL_ERROR, 'Additional history output file specified, but does not exist: ', trim(his_output_file))
         end if
      else
         Leho = .false.
      end if

      ! check if statistics file exists
      if (statistics_file /= ' ') then
         inquire (file=statistics_file, exist=Lstt)
         if (.not. Lstt) then
            call mess(LEVEL_ERROR, 'Statistics file does not exist: ', trim(statistics_file))
         end if
      end if

      ! check if proc_def file exists
      if (proc_def_file /= ' ') then
         inquire (file=proc_def_file, exist=Lpdf)
         if (.not. Lpdf) then
            call mess(LEVEL_ERROR, 'Specified process library file does not exist: ', trim(proc_def_file))
         end if
      else
         proc_def_file = trim(share_dir)//'proc_def.dat'
         inquire (file=proc_def_file, exist=Lpdf)
         if (Lpdf) then
            call mess(LEVEL_INFO, 'Using default Process library file: ', trim(proc_def_file))
         else
            call mess(LEVEL_ERROR, 'Default process library file does not exist: ', trim(proc_def_file))
         end if
      end if

      ! check if open process dll/so file exists
      if (proc_dllso_file /= ' ') then
         inquire (file=proc_dllso_file, exist=Lopl)
         if (.not. Lopl) then
            call mess(LEVEL_ERROR, 'Open process library dll/so file does not exist: ', trim(proc_dllso_file))
         end if
      end if

      ! check if bloom file exists
      if (bloom_file /= ' ') then
         inquire (file=bloom_file, exist=Lblm)
         if (.not. Lblm) then
            call mess(LEVEL_ERROR, 'Specified BLOOM species definition file does not exist: ', trim(bloom_file))
         end if
      else
         bloom_file = trim(share_dir)//'bloom.spe'
         inquire (file=bloom_file, exist=Lblm)
         if (Lblm) then
            call mess(LEVEL_INFO, 'Using default BLOOM species definition file: ', trim(bloom_file))
         else
            call mess(LEVEL_ERROR, 'Default BLOOM species definition file does not exist: ', trim(bloom_file))
         end if
      end if

      ! water column definition
      if (kmx > 0) then
         call getkbotktopmax(1, kbx, kt, kdum)
         call getkbotktopmax(Ndxi, kdum, kt, ktx)
         num_cells = ktx - kbx + 1 ! includes D-Flow FM dummy layer
         num_exchanges_u_dir = 0
         num_exchanges_v_dir = 0
         num_exchanges_z_dir = num_cells - 2 * Ndxi + 1
         num_exchanges_bottom_dir = 0

         ! allocate vertical exchanges array
         call realloc(iexpnt, 4 * num_exchanges_z_dir, keepExisting=.false., fill=0)

         ! set vertical exchanges
         iex = 0
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            do k = ktmax, kb + 1, -1
               iex = iex + 1
               iexpnt(1 + 4 * (iex - 1)) = k - kbx + 1
               iexpnt(2 + 4 * (iex - 1)) = k - 1 - kbx + 1
            end do
         end do
      else
         kbx = 1
         ktx = Ndxi
         num_cells = Ndxi
         num_exchanges_u_dir = 0
         num_exchanges_v_dir = 0
         num_exchanges_z_dir = 0
         num_exchanges_bottom_dir = 0

         ! allocate vertical exchanges array (dummy)
         call realloc(iexpnt, 4 * num_exchanges_z_dir, keepExisting=.false., fill=0)
      end if

      ! allocate array that indicates if processes are active based on volume and depth criteria
      call realloc(wqactive, ktx, keepExisting=.false., fill=.true.)

      ! allocate array that indicates if processes are active based on 'ProcessesInactive' parameter
      call realloc(wqdoproc, ktx, keepExisting=.false., fill=.true.)

      ! allocate iknmrk array used by processes that indicates if processes are active, and position in the watercolumn, dynamically set every timestep
      call realloc(iknmrk, num_cells, keepExisting=.false., fill=0)

      ! allocate and fill array that can tell processes if they are in the current domain when running in parallel
      call realloc(wqmydomain, num_cells, keepExisting=.false., fill=.true.)
      if (jampi == 1) then
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            do k = kb, ktmax
               wqmydomain(k - kbx + 1) = idomain(kk) == my_rank
            end do
         end do
      end if

      !    store the number of segments and number of layers for access
      !    in the processes library
      call store_noseg_nolay(num_cells, kmx)

      ! ======================
      ! Start initialising WAQ
      ! ======================

      !     Read the substance file for the process defintion
      !     Reset number of messages
      ierr = 0

      call mess(LEVEL_INFO, 'Opening substance file: ', trim(substance_file))
      call set_log_unit_number(mdia)
      Lallocated = .false.

      if (.not. allocated(syname_sub)) then
         allocate (syname_sub(0))
      end if
      if (.not. allocated(syunit_sub)) then
         allocate (syunit_sub(0))
      end if
      if (.not. allocated(coname_sub)) then
         allocate (coname_sub(0))
      end if
      if (.not. allocated(covalue_sub)) then
         allocate (covalue_sub(0))
      end if
      if (.not. allocated(ouname_sub)) then
         allocate (ouname_sub(0))
      end if
      if (.not. allocated(oudesc_sub)) then
         allocate (oudesc_sub(0))
      end if
      if (.not. allocated(syname_eho)) then
         allocate (syname_eho(0))
      end if
      if (.not. allocated(syunit_eho)) then
         allocate (syunit_eho(0))
      end if
      if (.not. allocated(coname_eho)) then
         allocate (coname_eho(0))
      end if
      if (.not. allocated(covalue_eho)) then
         allocate (covalue_eho(0))
      end if
      if (.not. allocated(ouname_eho)) then
         allocate (ouname_eho(0))
      end if
      if (.not. allocated(oudesc_eho)) then
         allocate (oudesc_eho(0))
      end if
      if (.not. allocated(syname)) then
         allocate (syname(0))
      end if
      if (.not. allocated(coname)) then
         allocate (coname(0))
      end if
      if (.not. allocated(funame)) then
         allocate (funame(0))
      end if
      if (.not. allocated(paname)) then
         allocate (paname(0))
      end if
      if (.not. allocated(sfunname)) then
         allocate (sfunname(0))
      end if
      if (.not. allocated(diname)) then
         allocate (diname(0))
      end if
      if (.not. allocated(vename)) then
         allocate (vename(0))
      end if
      if (.not. allocated(flxdmp)) then
         allocate (flxdmp(0, 0, 0))
      end if
      if (.not. allocated(idpnew)) then
         allocate (idpnew(0))
      end if
      if (.not. allocated(dispnw)) then
         allocate (dispnw(0, 0))
      end if
      if (.not. allocated(dspx)) then
         allocate (dspx(0))
      end if
      if (.not. allocated(dsto)) then
         allocate (dsto(0))
      end if
      if (.not. allocated(velx)) then
         allocate (velx(0, 0))
      end if
      if (.not. allocated(vsto)) then
         allocate (vsto(0))
      end if

      call read_substances(Lallocated, substance_file, num_substances_transported, num_substances_total, num_constants, noout_sub, syname_sub, syunit_sub, coname_sub, &
                           covalue_sub, ouname_sub, oudesc_sub, ierr_sub, cerr)
      if (ierr_sub /= 0) call mess(LEVEL_ERROR, cerr)
      call realloc(syname_sub, num_substances_total, keepExisting=.false., fill=' ')
      call realloc(syunit_sub, num_substances_total, keepExisting=.false., fill=' ')
      call realloc(coname_sub, num_constants, keepExisting=.false., fill=' ')
      call realloc(covalue_sub, num_constants, keepExisting=.false., fill=0.0e0)
      call realloc(ouname_sub, noout_sub, keepExisting=.false., fill=' ')
      call realloc(oudesc_sub, noout_sub, keepExisting=.false., fill=' ')
      Lallocated = .true.
      call read_substances(Lallocated, substance_file, num_substances_transported, num_substances_total, num_constants, noout_sub, syname_sub, syunit_sub, coname_sub, &
                           covalue_sub, ouname_sub, oudesc_sub, ierr_sub, cerr)
      if (ierr_sub /= 0) call mess(LEVEL_ERROR, cerr)
      noout_map = noout_sub

      if (Leho) then
         call mess(LEVEL_INFO, 'Opening extra history output file: ', trim(his_output_file))
         Lallocated = .false.
         call read_substances(Lallocated, his_output_file, nosys_eho, notot_eho, nocons_eho, noout_eho, syname_eho, syunit_eho, coname_eho, &
                              covalue_eho, ouname_eho, oudesc_eho, ierr_eho, cerr)
         if (ierr_eho /= 0) call mess(LEVEL_ERROR, cerr)
         if (nosys_eho == 0 .and. notot_eho == 0 .and. nocons_eho == 0) then
            call realloc(ouname_eho, noout_eho, keepExisting=.false., fill=' ')
            call realloc(oudesc_eho, noout_eho, keepExisting=.false., fill=' ')
            Lallocated = .true.
            call read_substances(Lallocated, his_output_file, nosys_eho, notot_eho, nocons_eho, noout_eho, syname_eho, syunit_eho, coname_eho, &
                                 covalue_eho, ouname_eho, oudesc_eho, ierr_eho, cerr)
            if (ierr_eho /= 0) call mess(LEVEL_ERROR, cerr)
         else
            ! Error: extra history outputfile contains other definitions that output alone!
         end if
      else
         ! Warning: extra history outputfile does not exist!
         noout_eho = 0
      end if

      !     The active substances should be initialised as 'constituents' in DFM.
      !     Initial concentration (fields), boundary conditions and additional (waste) loads
      !     should be specified in DFM
      call realloc(syname, num_substances_total)
      call realloc(syunit, num_substances_total)
      do i = 1, num_substances_total
         syname(i) = syname_sub(i)
         syunit(i) = syunit_sub(i)
      end do

      call realloc(amass, [num_substances_total, num_cells], keepExisting=.false., fill=0.0d0) !< mass array to be updated

      !     add corresponding tracers and bottom substances, if not already defined by initial and/or boundary conditions
      transformcoef = 0.0_hp
      call realloc(isys2trac, num_substances_total, keepExisting=.false., fill=0)
      do i = 1, num_substances_transported
         call add_bndtracer(trim(syname_sub(i)), syunit(i), isys2trac(i), janew)
      end do
      call realloc(isys2wqbot, num_substances_total, keepExisting=.false., fill=0)
      do i = num_substances_transported + 1, num_substances_total
         call add_wqbot(trim(syname_sub(i)), syunit(i), isys2wqbot(i), janew)
      end do

      !     Additional  data that comes from DFM should be added to the parameter/function/segment function list before the wq_processes_initialise call

      !     No spatial parameters for now, they should come from DFM
      num_spatial_parameters = 0
      call realloc(paname, num_spatial_parameters)

      !      Use functions to set 2D (or 0D variables) from DFM per column
      num_time_functions = 0
      call realloc(funame, num_time_functions)

      num_spatial_time_fuctions = 0
      call realloc(sfunname, num_time_functions)

      call dfm_waq_initexternalforcings(ierr)
      if (ierr /= 0) then
         call mess(LEVEL_ERROR, 'Error reading water quality processes external forcings from ext-file')
      end if
      nosfunext = num_spatial_time_fuctions

      jawaqproc = 1 ! substances succesfully initiated

      if (timon) call timstop(ithndl)
   end subroutine fm_wq_processes_ini_sub

   module subroutine fm_wq_processes_ini_proc()
      use m_getkbotktopmax
      use m_fm_wq_processes
      use m_wq_processes_initialise
      use m_wq_processes_pmsa_size
      use bloom_data_vtrans
      use m_alloc
      use m_flow, only: kmx
      use m_flowgeom, only: Ndxi, ba
      use m_flowparameters, only: jasal, jatem, jawave, jawaveSwartDelwaq
      use fm_external_forcings_data
      use m_transport
      use m_partitioninfo
      use unstruc_model
      use unstruc_files
      use m_flowtimes
      use timers
      use m_wind, only: jawind, jarain, solrad_available
      use date_time_utils, only: compute_reference_day
      use m_logger_helper, only: set_log_unit_number

      implicit none

      type(procespropcoll) :: statprocesdef !< the statistical proces definition
      integer(4), parameter :: nomult = 0 !< number of multiple substances
      integer(4) :: imultp(2, nomult) !< multiple substance administration
      type(t_waq_item) :: constants !< delwaq constants list
      integer :: nocons_used !< number of constants actually used
      integer :: noinfo !< count of informative message
      integer :: nowarn !< count of warnings
      integer :: ierr, ierr2 !< error count

      integer(4) :: i, j, ip, icon, ipar, ifun, isfun, ivar
      integer :: ipoifmlayer, ipoifmktop, ipoifmkbot
      integer(4) :: refdayNr ! reference day number, varying from 1 till 365

      integer :: iex
      integer :: kk, k, kb, kt, ktmax

      integer :: lunlsp

      integer(4), save :: ithndl = 0

      character(len=20), parameter :: ctauflow = 'tauflow'
      character(len=20), parameter :: ctau = 'tau'
      character(len=20), parameter :: cvelocity = 'velocity'
      character(len=20), parameter :: csalinity = 'salinity'
      character(len=20), parameter :: ctemperatureflow = 'tempflow'
      character(len=20), parameter :: ctemperature = 'temp'
      character(len=20), parameter :: cwind = 'vwind'
      character(len=20), parameter :: cwinddir = 'winddir'
      character(len=20), parameter :: cfetchl = 'fetch'
      character(len=20), parameter :: cfetchd = 'initdepth'
      character(len=20), parameter :: cirradiation = 'radsurf'
      character(len=20), parameter :: crain = 'rain'
      character(len=20), parameter :: cvtrans = 'ACTIVE_vtrans'
      character(len=20), parameter :: cvertdisp = 'ACTIVE_vertdisp'
      character(len=10), parameter :: cbloom = 'd40blo'
      character(len=20), parameter :: cdoprocesses = 'DoProcesses'
      character(len=20), parameter :: cprocessesinactive = 'ProcessesInactive'

      character(len=20), parameter :: cWaveH = 'WaveHeight'
      character(len=20), parameter :: cWaveL = 'WaveLength'
      character(len=20), parameter :: cWaveT = 'WavePeriod'

      if (timon) call timstrt("fm_wq_processes_ini_proc", ithndl)

      !     try to open the lsp-file for logging output
      proc_log_file = defaultfilename('wq_lsp')
      open (newunit=lunlsp, file=proc_log_file, status='unknown', iostat=ierr)
      if (ierr /= 0) then
         call mess(LEVEL_ERROR, 'Could not open processes log file: ', trim(proc_log_file))
      end if
      call set_log_unit_number(lunlsp)
      !     Reset number of messages
      noinfo = 0
      nowarn = 0
      ierr = 0

      !     Use segment functions to set 3D (or 2D variables per column) from DFM (e.g. salinity or temperature)
      call mess(LEVEL_INFO, '==========================================================================')
      call mess(LEVEL_INFO, 'Data from hydrodynamics available for water quality')
      call mess(LEVEL_INFO, '--------------------------------------------------------------------------')
      num_spatial_time_fuctions = num_spatial_time_fuctions + 1
      isfsurf = num_spatial_time_fuctions
      call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='surf')
      call mess(LEVEL_INFO, '''horizontal surface'' connected as ''surf'' (by default)')

      icon = index_in_array(ctauflow, coname_sub)
      if (icon > 0) then
         num_spatial_time_fuctions = num_spatial_time_fuctions + 1
         isftau = num_spatial_time_fuctions
         call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='tauflow')
         call mess(LEVEL_INFO, '''bottom shear stress'' connected as ''tauflow''')
      else
         icon = index_in_array(ctau, coname_sub)
         if (icon > 0) then
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isftau = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='tau')
            call mess(LEVEL_INFO, '''bottom shear stress'' connected as ''tau''')
         else
            call mess(LEVEL_INFO, '''bottom shear stress'' not connected, because ''tauflow'' or ''tau'' are not in the sub-file.')
            isftau = 0
         end if
      end if
      if (isftau > 0) then
         if (jawaveSwartDelwaq == 0) then
            call mess(LEVEL_INFO, 'jawaveSwartDelwaq == 0 so tau/tauflow = taucur or tau from wave-current interaction if waves activated')
         else if (jawaveSwartDelwaq == 1) then
            call mess(LEVEL_INFO, 'jawaveSwartDelwaq == 1 so tau/tauflow = taucur + ftauw*tauwave')
         else if (jawaveSwartDelwaq == 2) then
            call mess(LEVEL_INFO, 'jawaveSwartDelwaq == 2 so tau/tauflow = taubxu')
         end if
      end if

      icon = index_in_array(cvelocity, coname_sub)
      if (icon > 0) then
         num_spatial_time_fuctions = num_spatial_time_fuctions + 1
         isfvel = num_spatial_time_fuctions
         call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='velocity')
         call mess(LEVEL_INFO, '''flow element center velocity'' connected as ''velocity''')
      else
         call mess(LEVEL_INFO, '''flow element center velocity'' not connected, because ''velocity'' is not in the sub-file.')
         isfvel = 0
      end if

      icon = index_in_array(cWaveH, coname_sub)
      isfwaveheight = 0
      if (icon > 0) then
         num_spatial_time_fuctions = num_spatial_time_fuctions + 1
         isfwaveheight = num_spatial_time_fuctions
         call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='WaveHeight') ! this must be the name used in waq, process input param/in sub file.
         call mess(LEVEL_INFO, '''flow element center WaveHeight'' connected as ''WaveHeight''')
      else
         call mess(LEVEL_INFO, '''flow element center WaveHeight'' not connected, because ''WaveHeight'' is not in the sub-file.')
         isfwaveheight = 0
      end if

      icon = index_in_array(cWaveL, coname_sub)
      isfwavelength = 0
      if (icon > 0) then
         num_spatial_time_fuctions = num_spatial_time_fuctions + 1
         isfwavelength = num_spatial_time_fuctions
         call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='WaveLength') ! this must be the name used in waq, process input param/in sub file.
         call mess(LEVEL_INFO, '''flow element center WaveLength'' connected as ''WaveLength''')
      else
         call mess(LEVEL_INFO, '''flow element center WaveLength'' not connected, because ''WaveLength'' is not in the sub-file.')
         isfwavelength = 0
      end if

      icon = index_in_array(cWaveT, coname_sub)
      isfwaveperiod = 0
      if (icon > 0) then
         num_spatial_time_fuctions = num_spatial_time_fuctions + 1
         isfwaveperiod = num_spatial_time_fuctions
         call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='WavePeriod') ! this must be the name used in waq, process input param/in sub file.
         call mess(LEVEL_INFO, '''flow element center WavePeriod'' connected as ''WavePeriod''')
      else
         call mess(LEVEL_INFO, '''flow element center WavePeriod'' not connected, because ''WavePeriod'' is not in the sub-file.')
         isfwaveperiod = 0
      end if

      icon = index_in_array(csalinity, coname_sub)
      isfsal = 0
      if (jasal == 1) then
         if (icon > 0) then
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isfsal = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='salinity')
            call mess(LEVEL_INFO, '''salinity'' connected as ''salinity''')
         else
            call mess(LEVEL_INFO, '''salinity'' not connected, because ''salinity'' is not in the sub-file.')
         end if
      else
         if (icon > 0) then
            call mess(LEVEL_INFO, '''salinity'' is the sub-file but ''salinity'' is not in the hydrodynamic model.')
         end if
      end if

      icon = index_in_array(ctemperatureflow, coname_sub)
      isftem = 0
      if (jatem >= 1) then
         if (icon > 0) then
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isftem = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='tempflow')
            call mess(LEVEL_INFO, '''temperature'' connected as ''tempflow''')
         else
            icon = index_in_array(ctemperature, coname_sub)
            if (icon > 0) then
               num_spatial_time_fuctions = num_spatial_time_fuctions + 1
               isftem = num_spatial_time_fuctions
               call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='temp')
               call mess(LEVEL_INFO, '''temperature'' connected as ''temp''')
            else
               call mess(LEVEL_INFO, '''temperature'' not connected, because ''tempflow'' or ''temp'' are not in the sub-file.')
            end if
         end if
      else
         if (icon > 0) then
            call mess(LEVEL_INFO, '''tempflow'' is the sub-file but ''temperature'' is not in the hydrodynamic model.')
         end if
         icon = index_in_array(ctemperature, coname_sub)
         if (icon > 0) then
            call mess(LEVEL_INFO, '''temp'' is the sub-file but ''temperature'' is not in the hydrodynamic model.')
         end if
      end if

      icon = index_in_array(cwind, coname_sub)
      isfvwind = 0
      if (jawind >= 1) then
         if (icon > 0) then
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isfvwind = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='vwind')
            call mess(LEVEL_INFO, '''wind velocity magnitude'' connected as ''vwind''')
         else
            call mess(LEVEL_INFO, '''wind velocity magnitude'' not connected, because ''vwind'' is not in the sub-file.')
         end if
      else
         if (icon > 0) then
            call mess(LEVEL_INFO, '''vwind'' is the sub-file but ''wind velocity'' is not in the hydrodynamic model.')
         end if
      end if

      icon = index_in_array(cwinddir, coname_sub)
      isfwinddir = 0
      if (jawind >= 1) then
         if (icon > 0) then
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isfwinddir = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='winddir')
            call mess(LEVEL_INFO, '''wind direction'' connected as ''winddir''')
         else
            call mess(LEVEL_INFO, '''wind direction'' not connected, because ''winddir'' is not in the sub-file.')
         end if
      else
         if (icon > 0) then
            call mess(LEVEL_INFO, '''winddir'' is the sub-file but ''wind direction'' is not in the hydrodynamic model.')
         end if
      end if

      icon = index_in_array(cfetchl, coname_sub)
      if (icon == 0) then
         icon = index_in_array(cfetchd, coname_sub)
      end if
      isffetchl = 0
      isffetchd = 0
      if (jawave == 1 .or. jawave == 2) then ! copied from "set_external_forcings", call to "tauwavefetch"
         if (icon > 0) then
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isffetchl = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='fetch')
            call mess(LEVEL_INFO, '''fetch length'' connected as ''fetch''')
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isffetchd = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='initdepth')
            call mess(LEVEL_INFO, '''fetch depth'' connected as ''initdepth''')
         else
            call mess(LEVEL_INFO, '''fetch length'' and ''fetch depth'' not connected, because neither ''fetch'' or ''initdepth'' is in the sub-file.')
         end if
      else
         if (icon > 0) then
            call mess(LEVEL_INFO, '''fetch'' or ''initdepth'' is the sub-file but ''fetch length''/''fetch depth'' are not in the hydrodynamic model.')
         end if
      end if

      icon = index_in_array(cirradiation, coname_sub)
      isfradsurf = 0
      if (solrad_available .and. jatem > 1) then
         if (icon > 0) then
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isfradsurf = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='radsurf')
            call mess(LEVEL_INFO, '''solar radiation'' connected as ''radsurf''')
         else
            call mess(LEVEL_INFO, '''solar radiation'' not connected, because ''radsurf'' is not in the sub-file.')
         end if
      else
         if (icon > 0) then
            call mess(LEVEL_INFO, '''radsurf'' is the sub-file but ''solar radiation'' is not in the hydrodynamic model.')
         end if
      end if

      icon = index_in_array(crain, coname_sub)
      isfrain = 0
      if (jarain == 1) then
         if (icon > 0) then
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isfrain = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='rain')
            call mess(LEVEL_INFO, '''rain'' (mm/day) connected as ''rain'' (mm/h)')
         else
            call mess(LEVEL_INFO, '''rain'' not connected, because ''rain'' is not in the sub-file.')
         end if
      else
         if (icon > 0) then
            call mess(LEVEL_INFO, '''rain'' is the sub-file but ''rain'' is not in the hydrodynamic model.')
         end if
      end if

      !     data only needed for vtrans
      icon = index_in_array(cvtrans, coname_sub)
      if (icon > 0) then
         icon = index_in_array(cvertdisp, coname_sub)
         if (icon <= 0) then
            call mess(LEVEL_ERROR, 'The water quality process ''vtrans'' requires the process ''vertdisp'' to be active.')
         else if (kmx == 0) then
            call mess(LEVEL_WARN, 'The water quality process ''vtrans'' was not switched on because the model is not 3D.')
         else
            fm_vtrans = .true.
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isfvertdisper = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='VertDisper')
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isffmlayer = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='FMLayer')
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isffmktop = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='FMkTop')
            num_spatial_time_fuctions = num_spatial_time_fuctions + 1
            isffmkbot = num_spatial_time_fuctions
            call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill='FMkBot')
            call mess(LEVEL_INFO, 'Found process ''vtrans'', added ''VertDisper'', ''FMLayer'', ''FMkTop'' and ''FMkBot''')
         end if
      end if
      call mess(LEVEL_INFO, '--------------------------------------------------------------------------')

      noconm = num_constants + 1000
      call realloc(coname, noconm)
      ierr2 = constants%initialize()
      ierr2 = constants%resize(noconm)

      !     Skip constants from the sub-file that will be added by DFM as parameter/function/segment function
      nocons_used = 0
      do i = 1, num_constants
         ipar = index_in_array(coname_sub(i), paname)
         if (ipar > 0) then
            call mess(LEVEL_INFO, 'Water quality constant replaced by spatial parameter: ', coname_sub(i))
         end if
         ifun = index_in_array(coname_sub(i), funame)
         if (ifun > 0) then
            call mess(LEVEL_INFO, 'Water quality constant replaced by temporal function: ', coname_sub(i))
         end if
         isfun = index_in_array(coname_sub(i), sfunname)
         if (isfun > 0) then
            call mess(LEVEL_INFO, 'Water quality constant replaced by segment function: ', coname_sub(i))
         end if
         if (ipar < 0 .and. ifun < 0 .and. isfun < 0) then
            nocons_used = nocons_used + 1
            coname(nocons_used) = coname_sub(i)
            constants%ipnt(nocons_used) = i
            constants%name(nocons_used) = coname_sub(i)
            constants%constant(nocons_used) = covalue_sub(i)
         end if
      end do
      num_constants = nocons_used
      constants%no_item = num_constants

      !     Set the required output data.
      !     -> When outputs are not specified, they might end up in A(1), a 'black hole' location that is constantly overwritten
      noout_user = noout_map + noout_eho
      noout = noout_user

      allocate (outputs%names(noout_user))
      allocate (outputs%std_var_name(noout_user))
      allocate (outputs%pointers(noout_user))
      allocate (outputs%units(noout_user))
      allocate (outputs%description(noout_user))

      outputs%current_size = noout_user
      do i = 1, noout_sub
         outputs%names(i) = ouname_sub(i)
         outputs%std_var_name(i) = ' '
         outputs%units(i) = ' '
         outputs%description(i) = trim(oudesc_sub(i))//' ('//trim(ouname_sub(i))//') '
         outputs%pointers(i) = -1
      end do
      do j = 1, noout_eho
         i = noout_map + j
         outputs%names(i) = ouname_eho(j)
         outputs%std_var_name(i) = ' '
         outputs%units(i) = ' '
         outputs%description(i) = trim(oudesc_eho(j))//' ('//trim(ouname_eho(j))//') '
         outputs%pointers(i) = -1
      end do

      deallocate (coname_sub)
      deallocate (covalue_sub)
      deallocate (ouname_sub)

      !     calculation timers need to be known for the statistical processes (start time/stop time)
      isfact = 1
      itfact = 86400
      itstrt_process = nint(tstart_user)
      if (ti_waqproc > 0) then
         itstop_process = floor(tstop_user / ti_waqproc + 0.001d0) * ti_waqproc
      else
         itstop_process = tstop_user
      end if
      otime = dble(julrefdat) - 0.5d0 !refdate_mjd

      !     Compute refday needed for daylight process
      call compute_reference_day(refdat, refdayNr)

      !     Finally, evaluate the processes using the proces library
      !     --------------------------------------------------------
      bloom_output_file = defaultfilename('bloom')

      call mess(LEVEL_INFO, 'Initialising water quality processes.')
      call wq_processes_initialise(lunlsp, proc_def_file, proc_dllso_file, bloom_file, bloom_output_file, statistics_file, statprocesdef, outputs, &
                                   nomult, imultp, constants, refdayNr, noinfo, nowarn, ierr)
      call mess(LEVEL_INFO, 'Number of warnings during initialisation of the processes : ', nowarn)
      call mess(LEVEL_INFO, 'Number of errors during initialisation of the processes   : ', ierr)
      if (ierr /= 0) then
         call mess(LEVEL_ERROR, 'Something went wrong during initialisation of the processes. Check the lsp-file: ', trim(proc_log_file))
      end if
      call mess(LEVEL_INFO, 'Water quality processes initialisation was successful')
      call mess(LEVEL_INFO, '==========================================================================')

      !     proces fractional step multiplier is 1 for all
      prondt = 1

      !     Allocate the work arrays for the pointers
      call realloc(process_space_int, process_space_int_len, keepExisting=.false., fill=0)
      call realloc(increm, process_space_int_len, keepExisting=.false., fill=0)

      !     allocate deriv array - holds all derivatives
      call realloc(deriv, [num_cells, num_substances_total], keepExisting=.false., fill=0.0d0) !< Model derivatives (= stochi(num_substances_total ,noflux) * flux(noflux, num_cells))

      !     Determine size of a array from process system and num_cells/num_exchanges_z_dir, and allocate it
      call wq_processes_pmsa_size(lunlsp, num_cells, num_exchanges_z_dir, sizepmsa)
      !     And actually allocate and zero the A array
      call realloc(process_space_real, sizepmsa, keepExisting=.false., fill=0.0)

      !     constants from the substance file
      ip = arrpoi(iicons)
      do i = 1, num_constants
         process_space_real(ip + i - 1) = constants%constant(i)
      end do

      !     defaults from the proces library
      ip = arrpoi(iidefa)
      do i = 1, num_defaults
         process_space_real(ip + i - 1) = defaul(i)
      end do

      !     parameters from ext-file
      !     spatially varying constants provided trough the ext-file that remain fixed during the run
      !      -> fill using abcdabcdabcd partern
      if (num_spatial_parameters > 0) then
         ip = arrpoi(iiparm)
         do k = 0, ktx - kbx
            do j = 1, num_spatial_parameters
               process_space_real(ip) = painp(j, k + kbx)
               ip = ip + 1
            end do
         end do
      end if

      !      functions from DFM, one value for the whole system
      !      -> can be used for timeseries that are updated by DFM, e.g. global irradiation
      !      -> or can be used for 2D values when calculating only one water column
      !      -> updated in waqfil

      !     exchange areas
      ip = arrpoi(iiarea)
      iex = 0
      do kk = 1, Ndxi
         call getkbotktopmax(kk, kb, kt, ktmax)
         do k = ktmax, kb + 1, -1
            process_space_real(ip + iex) = ba(kk)
            iex = iex + 1
         end do
      end do

      if (isffmlayer > 0) then
         ipoifmlayer = arrpoi(iisfun) + (isffmlayer - 1) * num_cells
         ipoifmktop = arrpoi(iisfun) + (isffmktop - 1) * num_cells
         ipoifmkbot = arrpoi(iisfun) + (isffmkbot - 1) * num_cells
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            do k = kb, ktmax
               process_space_real(ipoifmlayer + k - kbx) = ktmax - k + 1
               process_space_real(ipoifmktop + k - kbx) = ktmax - kbx + 1
               process_space_real(ipoifmkbot + k - kbx) = kb - kbx + 1
            end do
         end do
      end if

      bloom_status_ind = index_in_array(cbloom, pronam)
      if (bloom_status_ind > 0) then
         bloom_ind = prvpnt(bloom_status_ind)
         write (lunlsp, *) ' MESSAGE: Bloom fractional step switched on'
      else
         bloom_status_ind = 0
         bloom_ind = 0
      end if

      call realloc(waqoutputs, [noout, num_cells], keepExisting=.false., fill=-999.0d0)
      call realloc(outvar, noout, keepExisting=.false., fill=0)
      do j = 1, noout
         ivar = index_in_array(outputs%names(j), varnam)
         outvar(j) = ivar
      end do

      ! If there is a parameter 'doprocesses', mask the area where processes are active by setting wqdoproc to .true./.false. (default=.true.)
      ipar = index_in_array(cdoprocesses, paname)
      if (ipar > 0) then
         call mess(LEVEL_WARN, 'Found parameter ''DoProcesses'', but this is not used any more.')
         call mess(LEVEL_WARN, 'Use ''ProcessesInactive'' with non-zero values to set processes to inactive instead')
      end if

      ! If there is a parameter 'ProcessesInactive', mask the area where processes are active by setting wqdoproc to .true./.false. (default=.true.)
      ipar = index_in_array(cprocessesinactive, paname)
      if (ipar > 0) then
         call mess(LEVEL_INFO, 'Found parameter ''ProcessesInactive''. Water quality processes are switched off for segments where ProcessesInactive <> 0.0')
         do k = kbx, ktx
            wqdoproc(k) = comparereal(painp(ipar, k), 0.0e0) == 0
         end do
      end if

      jawaqproc = 2 ! processes succesfully initiated

      if (timon) call timstop(ithndl)
      return
   end subroutine fm_wq_processes_ini_proc

!! @return Integer result status (0 if successful)
   subroutine dfm_waq_initexternalforcings(iresult)
      use precision, only: dp
      use fm_external_forcings
      use m_alloc
      use fm_external_forcings_data
      use m_flowparameters
      use m_flowtimes
      use m_flowgeom
      use m_partitioninfo
      use unstruc_model
      use unstruc_messages, only: loglevel_StdOut, callback_msg
      use timespace
      use m_flow
      use m_meteo
      use m_ec_instance
      use dfm_error
      use m_fm_wq_processes
      use timers
      use unstruc_files, only: resolvePath
      use fm_location_types, only: UNC_LOC_S
      use m_delpol
      use m_get_kbot_ktop
      use m_find_name, only: find_name

      implicit none

      integer, intent(out) :: iresult

      character(len=256) :: filename, sourcemask
      integer :: kb, k, ja, method, kk, kt, lenqidnam, ipa, ifun, isfun, imna
      integer :: klocal, waqseg2D, waqseglay
      character(len=NAMTRACLEN) :: qidnam
      character(len=20) :: waqinput
      real(kind=dp), allocatable :: viuh(:) ! temporary variable

      integer(4), save :: ithndl = 0

      if (timon) call timstrt("dfm_waq_initexternalforcings", ithndl)

      iresult = DFM_NOERR

      success = .true. ! default if no valid providers are present in *.ext file (fm_external_forcings_data::success)

      if (.not. allocated(paname)) then
         allocate (paname(0))
      end if
      if (.not. allocated(funame)) then
         allocate (funame(0))
      end if
      if (.not. allocated(sfunname)) then
         allocate (sfunname(0))
      end if
      if (.not. allocated(monname)) then
         allocate (monname(0))
      end if
      if (.not. allocated(mondef)) then
         allocate (mondef(0, 0))
      end if

      call settimespacerefdat(refdat, julrefdat, Tzone, Timjan)

      if (mext /= 0) then
         ja = 1

         do while (ja == 1) ! read *.ext file

            call delpol() ! remove a possibly existing polygon
            call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, sourcemask)
            if (ja == 1) then
               call resolvePath(filename, md_extfile_dir)
               call mess(LEVEL_INFO, 'External Forcing or Initialising '''//trim(qid)//''' from file '''//trim(filename)//'''.')
               ! Initialize success to be .false.
               success = .false.

               qidnam = qid
               call get_waqinputname(qid, waqinput, qidnam)
               lenqidnam = len_trim(qidnam)
               if (filetype == 7 .and. method == 4) then
                  method = 5 ! upward compatible fix
               end if

               if (qid(1:12) == 'waqparameter') then
                  ipa = find_name(paname, waqinput)

                  if (ipa == 0) then
                     num_spatial_parameters = num_spatial_parameters + 1
                     ipa = num_spatial_parameters
                     call realloc(paname, num_spatial_parameters, keepExisting=.true., fill=waqinput)
                     call realloc(painp, [num_spatial_parameters, Ndkx], keepExisting=.true., fill=0.0)
                  end if
                  call realloc(viuh, Ndkx, keepExisting=.false., fill=dmiss)

                  !  copy existing parameter values (if they existed) in temp array
                  do kk = 1, Ndxi
                     call getkbotktop(kk, kb, kt)
                     viuh(kk) = painp(ipa, kk)
                     do k = kb, kb + kmxn(kk) - 1
                        viuh(k) = painp(ipa, k)
                     end do
                  end do

                  ! will only fill 2D part of viuh
                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

                  if (success) then
                     do kk = 1, Ndxi
                        if (viuh(kk) /= dmiss) then
                           painp(ipa, kk) = viuh(kk)
                           call getkbotktop(kk, kb, kt)
                           do k = kb, kb + kmxn(kk) - 1
                              painp(ipa, k) = painp(ipa, kk)
                           end do
                        end if
                     end do
                  end if
                  deallocate (viuh)

               elseif (qid(1:16) == 'waqsegmentnumber') then
                  ipa = find_name(paname, waqinput)

                  if (ipa == 0) then
                     num_spatial_parameters = num_spatial_parameters + 1
                     ipa = num_spatial_parameters
                     call realloc(paname, num_spatial_parameters, keepExisting=.true., fill=waqinput)
                     call realloc(painp, [num_spatial_parameters, Ndkx], keepExisting=.true., fill=0.0)
                  end if
                  call realloc(viuh, Ndkx, keepExisting=.false., fill=dmiss)

                  ! copy existing parameter values (if they existed) in temp array
                  do kk = 1, Ndxi
                     call getkbotktop(kk, kb, kt)
                     viuh(kk) = painp(ipa, kk)
                     do k = kb, kb + kmxn(kk) - 1
                        viuh(k) = painp(ipa, k)
                     end do
                  end do

                  ! will only fill 2D part of viuh
                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

                  if (success) then
                     do kk = 1, Ndxi
                        if (viuh(kk) /= dmiss) then
                           if (jampi == 0) then
                              waqseg2D = mod(int(viuh(kk)) - 1, Ndxi) + 1
                              waqseglay = (int(viuh(kk)) - 1) / Ndxi + 1
                           else
                              waqseg2D = mod(int(viuh(kk)) - 1, Nglobal_s) + 1
                              waqseglay = (int(viuh(kk)) - 1) / Nglobal_s + 1
                           end if
                           klocal = global_to_local(waqseg2D)
                           if (klocal >= 1 .and. klocal <= Ndxi .and. waqseglay >= 1 .and. waqseglay <= kmx) then
                              call getkbotktop(klocal, kb, kt)
                              painp(ipa, kk) = max(kb, kb + kmxn(kk) - waqseglay) - kbx + 1
                           else
                              painp(ipa, kk) = -999.0
                           end if
                           call getkbotktop(kk, kb, kt)
                           do k = kb, kb + kmxn(kk) - 1
                              painp(ipa, k) = painp(ipa, kk)
                           end do
                        end if
                     end do
                  end if
                  deallocate (viuh)

               else if (qid(1:11) == 'waqfunction') then

                  ifun = find_name(funame, waqinput)

                  if (ifun == 0) then
                     num_time_functions = num_time_functions + 1
                     call realloc(funame, num_time_functions, keepExisting=.true., fill=waqinput)
                     call reallocP(funinp, [num_time_functions, 1], keepExisting=.true., fill=0.0d0)
                  end if
                  success = .true.

               else if (qid(1:18) == 'waqsegmentfunction') then

                  isfun = find_name(sfunname, waqinput)

                  if (isfun == 0) then
                     num_spatial_time_fuctions = num_spatial_time_fuctions + 1
                     call realloc(sfunname, num_spatial_time_fuctions, keepExisting=.true., fill=waqinput)
                     call reallocP(sfuninp, [num_spatial_time_fuctions, Ndkx], keepExisting=.true., fill=0.0d0)
                  end if
                  success = .true.

               else if (qid(1:17) == 'waqmonitoringarea') then
                  imna = find_name(monname, waqinput)

                  if (imna == 0) then
                     nomon = nomon + 1
                     imna = nomon
                     call realloc(monname, nomon, keepExisting=.true., fill=waqinput)
                     call realloc(mondef, [nomon, Ndkx], keepExisting=.true., fill=2)
                  end if

                  call realloc(viuh, Ndkx, keepExisting=.false., fill=dmiss)

                  ! will only fill 2D part of viuh
                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

                  if (success) then
                     do kk = 1, Ndxi
                        if (viuh(kk) /= dmiss) then
                           mondef(imna, kk) = 1
                           call getkbotktop(kk, kb, kt)
                           do k = kb, kb + kmxn(kk) - 1
                              mondef(imna, k) = 1
                           end do
                        end if
                     end do
                  end if
                  deallocate (viuh)
               else
                  ! just accept any other keyword as success, they are evaluated again in unstruc.F90
                  success = .true.
               end if

            end if

         end do

      end if ! read mext file

      if (loglevel_StdOut == LEVEL_DEBUG .and. associated(ecInstancePtr)) then
         call ecInstancePrintState(ecInstancePtr, callback_msg, LEVEL_DEBUG)
      end if

      if (.not. success) then
         iresult = DFM_EXTFORCERROR
      end if

      if (mext /= 0) then
         rewind (mext) ! rewind ext file
      end if

      if (timon) call timstop(ithndl)
      return

   contains

      function findloc(array, value)
         ! Private version of findloc - this intrinsic is new in Fortran 2008
         ! and not supported by gfortran 4.9.1
         integer, dimension(:), intent(in) :: array
         integer, intent(in) :: value

         integer, dimension(1) :: findloc

         integer :: i

         findloc = 0

         do i = 1, size(array)
            if (array(i) == value) then
               findloc = i
               exit
            end if
         end do
      end function findloc

      integer function global_to_local(global_number)
         !> Internal function: Translate the global segment number to the
        !! number used within the current domain
         use m_partitioninfo, only: jampi, iglobal_s
         use timers

         integer, intent(in) :: global_number !< Global segment number to be translated

         integer, save :: previous_global = -1
         integer, save :: previous_local = -1
         integer :: i(1)

         integer(4), save :: ithndl = 0

         if (timon) call timstrt("global_to_local", ithndl)

         if (jampi == 0) then
            global_to_local = global_number
         else
            global_to_local = dmiss
            if (global_number == previous_global) then
               global_to_local = previous_local
            else
               i = findloc(iglobal_s, value=global_number)
               if (i(1) > 0) then
                  global_to_local = i(1)
                  previous_global = global_number
                  previous_local = i(1)
               end if
            end if
         end if
         if (timon) call timstop(ithndl)
      end function global_to_local

   end subroutine dfm_waq_initexternalforcings

   module subroutine get_waqinputname(qid, inputname, qidname)
      !> Convert qid (from .ext file) to waq input name (split in generic qidname and specific input name).
    !! If the input qid is not waq input name, then the same qid is returned (and no waq input name)
      implicit none

      character(len=*), intent(in) :: qid !< Original quantityid, e.g., 'waqfunctionradsurf'.
      character(len=*), intent(inout) :: inputname !< The trimmed waq input name, e.g., 'fluor'.
      character(len=*), intent(inout) :: qidname !< The base input name for further use in external file analisys, e.g., 'tracerbnd'.

      character(len=256) :: qidloc !< Original quantityid, e.g., 'waqfunctionradsurf'.

      inputname = ''
      qidloc = qid
      if (qidloc(1:13) == 'initialwaqbot') then
         qidname = qidloc(1:13)
         if (len_trim(qidloc) > 13) then
            inputname = trim(qidloc(14:))
         end if
      else if (qidloc(1:11) == 'waqfunction') then
         qidname = qidloc(1:11)
         if (len_trim(qidloc) > 11) then
            inputname = trim(qidloc(12:))
         end if
      else if (qidloc(1:16) == 'waqsegmentnumber') then
         qidname = qidloc(1:16)
         if (len_trim(qidloc) > 16) then
            inputname = trim(qidloc(17:))
         end if
      else if (qidloc(1:18) == 'waqsegmentfunction') then
         qidname = qidloc(1:18)
         if (len_trim(qidloc) > 18) then
            inputname = trim(qidloc(19:))
         end if
      else if (qidloc(1:12) == 'waqparameter') then
         qidname = qidloc(1:12)
         if (len_trim(qidloc) > 12) then
            inputname = trim(qidloc(13:))
         end if
      else if (qidloc(1:18) == 'waqmassbalancearea') then
         qidname = qidloc(1:18)
         if (len_trim(qidloc) > 18) then
            inputname = trim(qidloc(19:))
         end if
      else if (qidloc(1:17) == 'waqmonitoringarea') then
         qidname = qidloc(1:17)
         if (len_trim(qidloc) > 17) then
            inputname = trim(qidloc(18:))
         end if
      end if

      return
   end subroutine get_waqinputname

   subroutine add_wqbot(wqbotnam, wqbotunit, iwqbot, janew)
      !> add waq bottom substance
      use m_flowgeom
      use m_flow, only: Ndkx
      use fm_external_forcings_data, only: trnames
      use m_fm_wq_processes
      use m_alloc
      use m_missing
      use messagehandling, only: LEVEL_ERROR, mess
      use m_find_name, only: find_name
      implicit none

      character(len=*), intent(in) :: wqbotnam
      character(len=20), intent(in) :: wqbotunit
      integer, intent(out) :: iwqbot
      integer, intent(out) :: janew

      integer :: itrac

      if (.not. allocated(wqbotnames)) then
         allocate (wqbotnames(0))
      end if
      if (.not. allocated(wqbot)) then
         allocate (wqbot(0, 0))
      end if
      if (.not. allocated(trnames)) then
         allocate (trnames(0))
      end if

      iwqbot = find_name(wqbotnames, wqbotnam)
      itrac = find_name(trnames, wqbotnam)

      if (itrac /= 0) then
         call mess(LEVEL_ERROR, 'add_wqbot: water quality bottom variable named '''//trim(wqbotnam)//''' already exists as a tracer.')
      end if

      janew = 0
      if (iwqbot == 0) then
         janew = 1
         !     add bottom substance

         numwqbots = numwqbots + 1
         iwqbot = numwqbots

         !     realloc
         call realloc(wqbotnames, numwqbots, keepExisting=.true., fill=trim(wqbotnam))
         call realloc(wqbotunits, numwqbots, keepExisting=.true., fill=wqbotunit)
         call realloc(wqbot, (/numwqbots, Ndkx/), keepExisting=.true., fill=0.0d0)
      end if
   end subroutine add_wqbot

   module subroutine fm_wq_processes_step(dt, time)
      use precision, only: dp
      use m_fm_wq_processes
      use m_wq_processes_proces
      use m_mass_balance_areas
      use m_flow, only: vol1
      use timers

      implicit none

      real(kind=dp), intent(in) :: dt !< timestep for waq in seconds
      real(kind=dp), intent(in) :: time !< time     for waq in seconds

      integer :: ipoiconc

      integer :: ipoivol, ipoisurf, ipoiarea
      integer :: ipoivelx, ipoidefa, ipoiflux

      integer(4), save :: ithand0 = 0
      integer(4), save :: ithand1 = 0
      integer(4), save :: ithand2 = 0

      if (jawaqproc == 0) then
         return
      end if

      if (timon) call timstrt("fm_wq_processes_step", ithand0)

      !     copy data from D-FlowFM to WAQ
      if (timon) call timstrt("copy_data_from_fm_to_wq_processes", ithand1)
      call copy_data_from_fm_to_wq_processes(time)
      if (timon) call timstop(ithand1)

      ipoiconc = arrpoi(iiconc)
      ipoivol = arrpoi(iivol)
      ipoivelx = arrpoi(iivelx)
      ipoidefa = arrpoi(iidefa)
      ipoiflux = arrpoi(iiflux)
      ipoisurf = arrpoi(iisfun) + (isfsurf - 1) * num_cells
      ipoiarea = arrpoi(iiarea)

      process_space_real(ipoidefa + 1) = time

      call wq_processes_proces(num_substances_total, num_cells, process_space_real(ipoiconc), vol1(kbx:ktx - kbx), time, dt, deriv, ndmpar, &
                               num_processes_activated, num_fluxes, process_space_int, prvnio, promnr, iflux, increm, process_space_real(ipoiflux), flxdmp, stochi, &
                               ibflag, bloom_status_ind, bloom_ind, amass, num_substances_transported, isfact, itfact, iexpnt, iknmrk, num_exchanges_u_dir, &
                               num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir, process_space_real(ipoiarea), num_dispersion_arrays_new, idpnew, dispnw, num_dispersion_arrays_extra, dspx, &
                               dsto, num_velocity_arrays_new, ivpnw, num_velocity_arrays_extra, process_space_real(ipoivelx), vsto, mbadefdomain(kbx:ktx), &
                               process_space_real(ipoidefa), prondt, prvvar, prvtyp, vararr, varidx, arrpoi, arrknd, arrdm1, &
                               arrdm2, num_vars, process_space_real, nomba, pronam, prvpnt, num_defaults, process_space_real(ipoisurf))

      ! copy data from WAQ to D-FlowFM
      if (timon) call timstrt("copy_data_from_wq_processes_to_fm", ithand2)
      call copy_data_from_wq_processes_to_fm(dt, time)
      if (timon) call timstop(ithand2)

      if (timon) call timstop(ithand0)
      return
   end subroutine fm_wq_processes_step

   subroutine copy_data_from_fm_to_wq_processes(time)
      !  copy data from D-FlowFM to WAQ
      use precision, only: dp
      use m_getfetch, only: getfetch
      use m_getkbotktopmax
      use m_flowgeom, only: Ndxi, ba
      use m_flow, only: vol1, ucx, ucy
      use m_flowtimes, only: irefdate, tunit
      use m_flowparameters, only: flowWithoutWaves, jawaveswartdelwaq
      use m_fm_wq_processes
      use m_transport, only: constituents, itemp, isalt
      use m_sferic, only: twopi, rd2dg
      use m_meteo
      use m_gettaus
      use m_gettauswave
      use m_get_kbot_ktop
      use m_get_link1
      implicit none

      real(kind=dp), intent(in) :: time !< time     for waq in seconds

      real(kind=dp) :: u10, dir, wdir, FetchL, FetchD
      integer :: isys, iconst, iwqbot
      integer :: ipoisurf, ipoitau, ipoivel
      integer :: ipoivol, ipoiconc, ipoisal, ipoitem
      integer :: ipoivwind, ipoiwinddir, ipoifetchl, ipoifetchd, ipoiradsurf, ipoirain, ipoivertdisper, ipoileng
      integer :: ipoiwaveheight, ipoiwavelength, ipoiwaveperiod
      integer :: ip, ifun, isfun
      integer :: kk, k, kb, kt, ktmax, ktwq
      integer :: L

      logical, save :: first = .true.

      if (num_time_functions > 0) then
         do ifun = 1, num_time_functions
            success = ec_gettimespacevalue(ecInstancePtr, item_waqfun(ifun), irefdate, tzone, tunit, time)
            if (.not. success) then
               call mess(LEVEL_ERROR, 'Error reading data for function: ', trim(funame(ifun)))
            end if
         end do
         ip = arrpoi(iifunc)
         do ifun = 1, num_time_functions
            process_space_real(ip + ifun - 1) = funinp(ifun, 1)
         end do
      end if

      if (nosfunext > 0) then
         do isfun = 1, nosfunext
            success = ec_gettimespacevalue(ecInstancePtr, item_waqsfun(isfun), irefdate, tzone, tunit, time)
            if (.not. success) then
               call mess(LEVEL_ERROR, 'Error reading data for segment function: ', trim(sfunname(isfun)))
            end if
         end do
         do isfun = 1, nosfunext
            ip = arrpoi(iisfun) + (isfun - 1) * num_cells
            do kk = 1, Ndxi
               call getkbotktopmax(kk, kb, kt, ktmax)
               do k = kb, ktmax
                  process_space_real(ip + k - kbx) = sfuninp(isfun, kk)
               end do
            end do
         end do
      end if

      ipoisurf = arrpoi(iisfun) + (isfsurf - 1) * num_cells
      do kk = 1, Ndxi
         call getkbotktopmax(kk, kb, kt, ktmax)
         do k = kb, ktmax
            process_space_real(ipoisurf + k - kbx) = ba(kk)
         end do
      end do

      ipoivol = arrpoi(iivol)
      do k = 0, ktx - kbx
         process_space_real(ipoivol + k) = vol1(k + kbx)
      end do

      if (isftau > 0) then
         ipoitau = arrpoi(iisfun) + (isftau - 1) * num_cells
         if (jawave == 0 .or. flowWithoutWaves) then
            call gettaus(1, 2)
         else
            call gettauswave(jawaveswartdelwaq)
         end if
         do kk = 1, Ndxi
            call getkbotktop(kk, kb, kt)
            process_space_real(ipoitau + kb - kbx) = taus(kk)
         end do
      end if

      if (isfvel > 0) then
         ipoivel = arrpoi(iisfun) + (isfvel - 1) * num_cells
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            do k = kb, ktmax
               process_space_real(ipoivel + k - kbx) = sqrt(ucx(k)**2 + ucy(k)**2)
            end do
         end do
      end if

      ! get wave params from dflow-fm and put to process_space_real array.
      if (isfwaveheight > 0) then
         ipoiwaveheight = arrpoi(iisfun) + (isfwaveheight - 1) * num_cells
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            do k = kb, ktmax
               process_space_real(ipoiwaveheight + k - kbx) = hwav(kk) * sqrt(2.0_fp)
            end do
         end do
      end if
      if (isfwavelength > 0) then
         ipoiwavelength = arrpoi(iisfun) + (isfwavelength - 1) * num_cells
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            do k = kb, ktmax
               process_space_real(ipoiwavelength + k - kbx) = rlabda(kk)
            end do
         end do
      end if
      if (isfwaveperiod > 0) then
         ipoiwaveperiod = arrpoi(iisfun) + (isfwaveperiod - 1) * num_cells
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            do k = kb, ktmax
               process_space_real(ipoiwaveperiod + k - kbx) = twav(kk)
            end do
         end do
      end if

      if (isfsal > 0) then
         ipoisal = arrpoi(iisfun) + (isfsal - 1) * num_cells
         do k = 0, ktx - kbx
            process_space_real(ipoisal + k) = constituents(isalt, k + kbx)
         end do
      end if

      if (isftem > 0) then
         ipoitem = arrpoi(iisfun) + (isftem - 1) * num_cells
         do k = 0, ktx - kbx
            process_space_real(ipoitem + k) = constituents(itemp, k + kbx)
         end do
      end if

      ! copy 2D arrays for wind velocity magnitude, fetch length, solar radiation and rain to 3D waq arrays, fill over whole column (safety)
      if (isfvwind > 0) then
         ipoivwind = arrpoi(iisfun) + (isfvwind - 1) * num_cells
         if (jawind == 1) then
            do kk = 1, Ndxi
               call getkbotktopmax(kk, kb, kt, ktmax)
               ! apparently wind is available at edges only, so just take the 1st edge
               call getlink1(kk, L)
               u10 = sqrt(wx(L) * wx(L) + wy(L) * wy(L))
               process_space_real(ipoivwind + kb - kbx:ipoivwind + ktmax - kbx) = u10
            end do
         else
            do k = 0, ktx - kbx
               process_space_real(ipoivwind + k) = windsp
            end do
         end if
      end if

      if (isfwinddir > 0) then
         ipoiwinddir = arrpoi(iisfun) + (isfwinddir - 1) * num_cells
         if (jawind == 1) then
            do kk = 1, Ndxi
               call getkbotktopmax(kk, kb, kt, ktmax)
               ! apparently wind is available at edges only, so just take the 1st edge
               call getlink1(kk, L)
               dir = atan2(wy(L), wx(L))
               if (dir < 0d0) dir = dir + twopi
               wdir = 270.0d0 - dir * rd2dg ! from rad to degree
               if (wdir < 0d0) wdir = wdir + 360.0d0
               process_space_real(ipoiwinddir + kb - kbx:ipoiwinddir + ktmax - kbx) = wdir
            end do
         else
            do k = 0, ktx - kbx
               process_space_real(ipoiwinddir + k) = winddir
            end do
         end if
      end if

      if (isffetchl > 0) then ! note: no fetch without wind
         ipoifetchl = arrpoi(iisfun) + (isffetchl - 1) * num_cells
         ipoifetchd = arrpoi(iisfun) + (isffetchd - 1) * num_cells
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            call getfetch(kk, U10, FetchL, FetchD)
            process_space_real(ipoifetchl + kb - kbx:ipoifetchl + ktmax - kbx) = FetchL
            process_space_real(ipoifetchd + kb - kbx:ipoifetchd + ktmax - kbx) = FetchD
         end do
      end if

      if (isfradsurf > 0) then
         ipoiradsurf = arrpoi(iisfun) + (isfradsurf - 1) * num_cells
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            call getkbotktop(kk, kb, kt)
            process_space_real(ipoiradsurf + kb - kbx:ipoiradsurf + ktmax - kbx) = solar_radiation(kk)
         end do
      end if

      if (isfrain > 0) then
         ipoirain = arrpoi(iisfun) + (isfrain - 1) * num_cells
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            process_space_real(ipoirain + kb - kbx:ipoirain + ktmax - kbx) = rain(kk) / 24.0d0 ! rain: mm/day => mm/h
         end do
      end if

      if (isfvertdisper > 0) then
         ipoivertdisper = arrpoi(iisfun) + (isfvertdisper - 1) * num_cells
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            do k = kb, ktmax
               process_space_real(ipoivertdisper + k - kbx) = vicwws(k - 1)
            end do
         end do
         ! set vertical dispersion lengths
         ipoileng = arrpoi(iileng)
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            do k = ktmax, kb + 1, -1
               process_space_real(ipoileng) = 0.5d0 * (zws(k) - zws(k - 1))
               ipoileng = ipoileng + 1
               process_space_real(ipoileng) = 0.5d0 * (zws(k - 1) - zws(k - 2))
               ipoileng = ipoileng + 1
            end do
         end do

      end if

      ! determine cells where processes can be active above a minimum volume and depth (the switch wqdoproc can overrule this)
      do kk = 1, Ndxi
         call getkbotktopmax(kk, kb, kt, ktmax)
         do k = kb, ktmax
            wqactive(k) = vol1(k) > waq_vol_dry_thr .and. (vol1(k) / ba(kk)) > waq_dep_dry_thr
         end do
      end do

      ! fill concentrations
      ipoiconc = arrpoi(iiconc)
      do k = kbx, ktx
         do isys = 1, num_substances_transported
            iconst = isys2const(isys)
            process_space_real(ipoiconc + (k - kbx) * num_substances_total + isys - 1) = constituents(iconst, k)
         end do
      end do

      ! fill masses (transported)
      do k = kbx, ktx
         if (wqactive(k)) then
            do isys = 1, num_substances_transported
               iconst = isys2const(isys)
               amass(isys, k - kbx + 1) = constituents(iconst, k) * vol1(k)
            end do
         else
            do isys = 1, num_substances_transported
               iconst = isys2const(isys)
               amass(isys, k - kbx + 1) = 0.0d0
            end do
         end if
      end do

      ! fill concentrations and masses (not transported, only first time)
      if (first) then
         first = .false.
         if (num_substances_total > num_substances_transported) then
            do kk = 1, Ndxi
               call getkbotktopmax(kk, kb, kt, ktmax)
               do k = kb, ktmax
                  do isys = num_substances_transported + 1, num_substances_total
                     iwqbot = isys2wqbot(isys)
                     process_space_real(ipoiconc + (k - kbx) * (num_substances_total) + isys - 1) = wqbot(iwqbot, k)
                     amass(isys, k - kbx + 1) = wqbot(iwqbot, k) * ba(kk)
                  end do
               end do
            end do
         end if
      end if

      ! set iknmrk array
      if (kmx > 0) then
         do kk = 1, Ndxi
            call getkbotktopmax(kk, kb, kt, ktmax)
            if (.not. wqdoproc(kb)) then
               ! whole column is inactive
               do k = kb, ktmax
                  iknmrk(k - kbx + 1) = IKNMRK_INACTIVE
               end do
            else
               ktwq = 0
               do k = ktmax, kb, -1
                  if (wqactive(k)) then
                     ! first active segment from the top
                     ktwq = k
                     exit
                  else
                     ! set inactive segments from the top as inactive
                     iknmrk(k - kbx + 1) = IKNMRK_INACTIVE
                  end if
               end do
               if (ktwq > 0) then
                  if (kb == ktwq) then
                     ! only one active segment
                     iknmrk(ktwq - kbx + 1) = IKNMRK_ACTIVE_TOPBOT
                  else
                     ! ktwq is the top active segment
                     iknmrk(ktwq - kbx + 1) = IKNMRK_ACTIVE_TOP
                     do k = kb + 1, ktwq - 1
                        iknmrk(k - kbx + 1) = IKNMRK_ACTIVE_MIDDLE
                     end do
                     iknmrk(kb - kbx + 1) = IKNMRK_ACTIVE_BOTTOM
                  end if
               end if
            end if
         end do
      else
         do k = 1, Ndxi
            if (wqactive(k) .and. wqdoproc(k)) then
               iknmrk(k) = IKNMRK_ACTIVE_TOPBOT
            else
               iknmrk(k) = IKNMRK_INACTIVE
            end if
         end do
      end if
      return
   end subroutine copy_data_from_fm_to_wq_processes

   subroutine copy_data_from_wq_processes_to_fm(dt, tim)
      !  copy data from WAQ to D-FlowFM
      use m_getkbotktopmax
      use m_missing, only: dmiss
      use m_flowgeom, only: Ndxi, ba
      use m_flow, only: vol1
      use m_flowtimes
      use m_flowparameters, only: eps10
      use m_fm_wq_processes
      use m_transport, only: constituents
      use precision_basics
      use timers

      implicit none

      real(kind=dp), intent(in) :: dt
      real(kind=dp), intent(in) :: tim

      integer :: isys, iconst, iwqbot
      integer :: ivar, iarr, iv_idx
      integer :: iarknd, ip_arr, idim1, idim2
      integer :: incr
      integer :: i, j, ip
      integer :: kk, k, kb, kt, ktmax
      logical :: copyoutput

      integer(4), save :: ithand1 = 0
      integer(4), save :: ithand2 = 0
      integer(4), save :: ithand3 = 0

      ! fill concentrations (transported)
      if (timon) call timstrt("copy_const", ithand1)
      do kk = 1, Ndxi
         call getkbotktopmax(kk, kb, kt, ktmax)
         do k = kb, kt
            if (wqactive(k)) then
               do isys = 1, num_substances_transported
                  iconst = isys2const(isys)
                  constituents(iconst, k) = amass(isys, k - kbx + 1) / vol1(k)
               end do
            end if
         end do
      end do
      if (timon) call timstop(ithand1)

      ! Ouputs to waq outputs array (only when his or map outputs will be written within the next timestep,
      ! and during first timestep)
      copyoutput = .false.
      if (comparereal(tim, tstart_user, eps10) == 0) then
         copyoutput = .true.
      end if
      if (ti_his > 0) then
         if (comparereal(tim + dt - 2.0_hp * eps10, time_his, eps10) >= 0) then
            copyoutput = .true.
         end if
      end if
      if (ti_map > 0 .or. ti_mpt(1) > 0) then
         if (comparereal(tim + dt - 2.0_hp * eps10, time_map, eps10) >= 0) then
            copyoutput = .true.
         end if
      end if

      if (copyoutput) then
         ! copy additional output
         if (timon) call timstrt("copy_output", ithand2)
         waqoutputs = dmiss
         noout = outputs%current_size
         do j = 1, noout
            ivar = outvar(j) ! which variable is it
            if (ivar > 0) then
               iarr = vararr(ivar) ! which array in process_space_real
               iv_idx = varidx(ivar) ! which index within the array
               iarknd = arrknd(iarr) ! which type of array (increm is 0, dim1 or 1)
               ip_arr = arrpoi(iarr) ! start point of the array in process_space_real
               idim1 = arrdm1(iarr) ! dimension in the 1st direction
               idim2 = arrdm2(iarr) ! dimension in the 2nd direction
               if (iarknd == 1) then
                  ip = ip_arr + iv_idx - 1
                  incr = 0
               elseif (iarknd == 2) then
                  ip = ip_arr + iv_idx - 1
                  incr = idim1
               elseif (iarknd == 3) then
                  ip = ip_arr + (iv_idx - 1) * idim1
                  incr = 1
               end if
               do i = 1, num_cells
                  waqoutputs(j, i) = process_space_real(ip)
                  ip = ip + incr
               end do
            end if
         end do
         if (timon) call timstop(ithand2)
      end if

      ! Copy wqbot data (when his or map, but also when rst or mba outputs will be written within the next timestep, and during first timestep)
      if (ti_rst > 0) then
         if (comparereal(tim + dt - 2.0_hp * eps10, time_rst, eps10) >= 0) then
            copyoutput = .true.
         end if
      end if
      if (ti_mba > 0) then
         if (comparereal(tim + dt - 2.0_hp * eps10, time_mba, eps10) >= 0) then
            copyoutput = .true.
         end if
      end if

      if (copyoutput) then
         ! copy concentrations (not transported)
         if (timon) call timstrt("copy_wqbot", ithand3)
         if (num_substances_total > num_substances_transported) then
            do kk = 1, Ndxi
               call getkbotktopmax(kk, kb, kt, ktmax)
               do k = kb, kt
                  do isys = num_substances_transported + 1, num_substances_total
                     iwqbot = isys2wqbot(isys)
                     wqbot(iwqbot, k) = amass(isys, k - kbx + 1) / ba(kk)
                  end do
               end do
            end do
         end if
         if (timon) call timstop(ithand3)
      end if
      return
   end subroutine copy_data_from_wq_processes_to_fm

   logical function wq_processes_mydomain(iseg)

      use m_fm_wq_processes
      use m_partitioninfo

      implicit none

      integer :: iseg
      if (jampi == 1) then
         if (iseg > 0 .and. iseg <= num_cells) then
            wq_processes_mydomain = wqmydomain(iseg)
         else
            wq_processes_mydomain = .false.
         end if
      else
         wq_processes_mydomain = .true.
      end if

   end function wq_processes_mydomain

   logical function reduce_sum_wq_processes(size_wq_processes_data, wq_processes_data)
      use precision, only: dp

      use m_partitioninfo

      implicit none

      integer :: size_wq_processes_data
      real :: wq_processes_data(size_wq_processes_data)

      real(kind=dp) :: mpi_wq_processes_data(size_wq_processes_data)
      real(kind=dp) :: mpi_wq_processes_data_reduce(size_wq_processes_data)

      if (jampi == 1) then
         mpi_wq_processes_data = dble(wq_processes_data)
         call reduce_double_sum(size_wq_processes_data, mpi_wq_processes_data, mpi_wq_processes_data_reduce)
         wq_processes_data = real(mpi_wq_processes_data_reduce)
      end if

      reduce_sum_wq_processes = .true.

   end function reduce_sum_wq_processes

   logical function reduce_int_max_wq_processes(wq_processes_data)

      use m_partitioninfo

      implicit none

      integer :: wq_processes_data

      if (jampi == 1) then
         call reduce_int1_max(wq_processes_data)
      end if

      reduce_int_max_wq_processes = .true.

   end function reduce_int_max_wq_processes

   module subroutine default_fm_wq_processes()
      !> defaults for process library (WAQ)
      use m_fm_wq_processes
      use unstruc_model
      implicit none

      jawaqproc = 0
      md_subfile = ''
      md_ehofile = ''
      md_sttfile = ''
      md_thetav_waq = 0d0
      md_dt_waqproc = 0d0

      return
   end subroutine default_fm_wq_processes

end submodule m_fm_wq_processes_sub_
