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

module fm_external_forcings
   use m_make_mirrorcells, only: make_mirrorcells
   use m_in2dflowcell, only: in2dflowcell
   use m_count_links, only: count_links
   use m_add_bndtracer, only: add_bndtracer
   use m_addopenbndsection, only: addopenbndsection
   use m_setwindstress, only: setwindstress
   use m_setsigmabnds, only: setsigmabnds
   use precision_basics, only: hp, dp
   use fm_external_forcings_utils, only: get_tracername, get_sedfracname
   use messagehandling, only: msgbuf, msg_flush, err_flush, LEVEL_WARN, mess
   implicit none

   private

   public set_external_forcings_boundaries, allocatewindarrays, adduniformtimerelation_objects, flow_initexternalforcings, findexternalboundarypoints

   integer, parameter :: max_registered_item_id = 128
   integer :: max_ext_bnd_items = 64 ! Starting size, will grow dynamically when needed.
   character(len=max_registered_item_id), allocatable :: registered_items(:)
   integer :: num_registered_items = 0

   interface
      module subroutine set_external_forcings_boundaries(time, iresult)
         real(kind=dp), intent(in) :: time !< current simulation time (s)
         integer, intent(out) :: iresult !< Integer error status
      end subroutine set_external_forcings_boundaries
   end interface

   interface
      module subroutine set_external_forcings(time_in_seconds, initialization, iresult)
         real(kind=dp), intent(in) :: time_in_seconds !< Time in seconds
         logical, intent(in) :: initialization !< initialization phase
         integer, intent(out) :: iresult !< Integer error status: DFM_NOERR==0 if succesful.
      end subroutine set_external_forcings
   end interface

   interface
      module subroutine init_new(external_force_file_name, iresult)
         character(len=*), intent(in) :: external_force_file_name !< file name for new external forcing boundary blocks
         integer, intent(inout) :: iresult
      end subroutine init_new
   end interface

   interface
      module subroutine init_old(iresult)
         integer, intent(inout) :: iresult
      end subroutine init_old
   end interface

   interface
      module subroutine init_misc(iresult)
         integer, intent(inout) :: iresult
      end subroutine init_misc
   end interface

   abstract interface
      subroutine fill_open_boundary_cells_with_inner_values_any(number_of_links, link2cell)
         integer, intent(in) :: number_of_links !< number of links
         integer, intent(in) :: link2cell(:, :) !< indices of cells connected by links
      end subroutine
   end interface

   public :: set_external_forcings
   public :: calculate_wind_stresses

   procedure(fill_open_boundary_cells_with_inner_values_any), pointer :: fill_open_boundary_cells_with_inner_values !< boundary update routine to be called

contains

!> print_error_message
   subroutine print_error_message(time_in_seconds)
      use m_ec_message, only: dumpECMessageStack
      use unstruc_messages, only: callback_msg

      real(kind=dp), intent(in) :: time_in_seconds !< Current time when doing this action

      character(len=255) :: tmpstr

      write (tmpstr, '(f22.11)') time_in_seconds
      call mess(LEVEL_WARN, 'Error while updating meteo/structure forcing at time='//trim(tmpstr))
      tmpstr = dumpECMessageStack(LEVEL_WARN, callback_msg)
   end subroutine print_error_message

!> prepare_wind_model_data
   subroutine prepare_wind_model_data(time_in_seconds, iresult)
      use m_wind
      use m_flowparameters, only: jawave, flowWithoutWaves
      use m_flow, only: wind_speed_factor
      use m_meteo
      use m_flowgeom, only: ln, lnx, ndx
      use precision_basics
      use m_flowparameters, only: eps10
      use m_physcoef, only: BACKGROUND_AIR_PRESSURE
      use dfm_error
      use m_tauwavefetch, only: tauwavefetch

      real(kind=dp), intent(in) :: time_in_seconds !< Current time when setting wind data
      integer, intent(out) :: iresult !< Error indicator

      integer :: ec_item_id, first, last, link, i, k
      logical :: first_time_wind

      wx = 0.d0
      wy = 0.d0
      wdsu_x = 0.d0
      wdsu_y = 0.d0
      wcharnock = 0.d0
      call initialize_array_with_zero(ec_pwxwy_x)
      call initialize_array_with_zero(ec_pwxwy_y)

      first_time_wind = (id_last_wind < 0)
      if (first_time_wind) then
         first = 1
         last = get_ec_number_of_items()
      else
         first = id_first_wind
         last = id_last_wind
      end if
      do i = first, last
         ec_item_id = get_ec_item_id(i)
         ! Retrieve wind's x- and y-component for ext-file quantity 'windxy'.
         if (ec_item_id == item_windxy_x .and. item_windxy_y /= ec_undef_int) then
            call get_timespace_value_by_item(item_windxy_x)
            ! Retrieve stress's x- and y-component for ext-file quantity 'stressxy'.
         elseif (ec_item_id == item_stressxy_x .and. item_stressxy_y /= ec_undef_int) then
            call get_timespace_value_by_item(item_stressxy_x)
            ! Retrieve wind's p-, x- and y-component for ext-file quantity 'airpressure_windx_windy'.
         else if (ec_item_id == item_apwxwy_p .and. item_apwxwy_x /= ec_undef_int .and. item_apwxwy_y /= ec_undef_int) then
            if (item_apwxwy_c /= ec_undef_int) then
               call get_timespace_value_by_name('airpressure_windx_windy_charnock')
            else
               call get_timespace_value_by_name('airpressure_windx_windy')
            end if
            ! Retrieve wind's charnock-component for ext-file quantity 'charnock'.
         else if (ec_item_id == item_charnock) then
            call get_timespace_value_by_item(item_charnock)
            ! Retrieve wind's x-component for ext-file quantity 'windx'.
         else if (ec_item_id == item_windx) then
            call get_timespace_value_by_item(item_windx)
            ! Retrieve wind's y-component for ext-file quantity 'windy'.
         else if (ec_item_id == item_windy) then
            call get_timespace_value_by_item(item_windy)
            ! Retrieve stress's x-component for ext-file quantity 'stressx'.
         else if (ec_item_id == item_stressx) then
            call get_timespace_value_by_item(item_stressx)
            ! Retrieve stress's y-component for ext-file quantity 'stressy'.
         else if (ec_item_id == item_stressy) then
            call get_timespace_value_by_item(item_stressy)
            ! Retrieve wind's p-component for ext-file quantity 'atmosphericpressure'.
         else if (ec_item_id == item_atmosphericpressure) then
            call get_timespace_value_by_item(item_atmosphericpressure)
         else
            cycle ! avoid updating id_first_wind and id_last_wind
         end if
         if (.not. success) then
            iresult = DFM_EXTFORCERROR
            call print_error_message(time_in_seconds)
            return
         end if
         if (first_time_wind) then
            id_first_wind = min(i, id_first_wind)
            id_last_wind = max(i, id_last_wind)
         end if
      end do

      if (jawindstressgiven > 0) then
         if (item_stressx /= ec_undef_int .and. item_stressy /= ec_undef_int) then
            call get_timespace_value_by_item_and_array(item_stressx, wdsu_x)
            call get_timespace_value_by_item_and_array(item_stressy, wdsu_y)
         else if (item_stressxy_x /= ec_undef_int .and. item_stressxy_y /= ec_undef_int) then
            call get_timespace_value_by_item_and_array(item_stressxy_x, wdsu_x)
            call get_timespace_value_by_item_and_array(item_stressxy_y, wdsu_y)
         end if
      end if

      if (allocated(ec_pwxwy_x) .and. allocated(ec_pwxwy_y)) then
         if (jawindstressgiven == 1) then
            call perform_additional_spatial_interpolation(wdsu_x, wdsu_y)
         else
            call perform_additional_spatial_interpolation(wx, wy)
         end if
         if (allocated(ec_pwxwy_c)) then
            do link = 1, lnx
               wcharnock(link) = wcharnock(link) + 0.5d0 * (ec_pwxwy_c(ln(1, link)) + ec_pwxwy_c(ln(2, link)))
            end do
         end if
      end if
      if (allocated(ec_charnock)) then
         do link = 1, lnx
            wcharnock(link) = wcharnock(link) + 0.5d0 * (ec_charnock(ln(1, link)) + ec_charnock(ln(2, link)))
         end do
      end if

      if (ja_wind_speed_factor > 0) then
         do link = 1, lnx
            if (wind_speed_factor(link) /= dmiss) then
               wx(link) = wx(link) * wind_speed_factor(link)
               wy(link) = wy(link) * wind_speed_factor(link)
            end if
         end do
      end if

      if (item_atmosphericpressure /= ec_undef_int) then
         do k = 1, ndx
            if (comparereal(patm(k), dmiss, eps10) == 0) then
               patm(k) = BACKGROUND_AIR_PRESSURE
            end if
         end do
      end if

      if ((jawave == 1 .or. jawave == 2) .and. .not. flowWithoutWaves) then
         call tauwavefetch(time_in_seconds)
      end if

      iresult = DFM_NOERR
   contains

!> get_ec_item_id
      integer function get_ec_item_id(i)
         integer, intent(in) :: i !< Input index

         get_ec_item_id = ecInstancePtr%ecItemsPtr(i)%ptr%id

      end function get_ec_item_id

!> ec_number_of_items
      integer function get_ec_number_of_items()

         get_ec_number_of_items = ecInstancePtr%nItems

      end function get_ec_number_of_items

!> get_timespace_value_by_name
      subroutine get_timespace_value_by_name(name)

         character(*), intent(in) :: name !< Input name

         success = ec_gettimespacevalue(ecInstancePtr, name, time_in_seconds)

      end subroutine get_timespace_value_by_name

!> get_timespace_value_by_item_and_array
      subroutine get_timespace_value_by_item_and_array(item, array)
         use m_flowtimes, only: irefdate, tzone, tunit

         integer, intent(in) :: item !< Input item
         real(kind=dp), intent(inout) :: array(:) !< Array that stores the obatained values

         success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds, array)

      end subroutine get_timespace_value_by_item_and_array

!> perform_additional_spatial_interpolation, the size of array_x and array_y is lnx.
      subroutine perform_additional_spatial_interpolation(array_x, array_y)

         real(kind=dp), intent(inout) :: array_x(:) !< Array of X-components for interpolation
         real(kind=dp), intent(inout) :: array_y(:) !< Array of Y-components for interpolation

         do link = 1, lnx
            array_x(link) = array_x(link) + 0.5d0 * (ec_pwxwy_x(ln(1, link)) + ec_pwxwy_x(ln(2, link)))
            array_y(link) = array_y(link) + 0.5d0 * (ec_pwxwy_y(ln(1, link)) + ec_pwxwy_y(ln(2, link)))
         end do

      end subroutine perform_additional_spatial_interpolation

!> get_timespace_value_by_item
      subroutine get_timespace_value_by_item(item)
         use m_flowtimes, only: irefdate, tzone, tunit
         integer, intent(in) :: item !< Input item

         success = ec_gettimespacevalue(ecInstancePtr, item, irefdate, tzone, tunit, time_in_seconds)

      end subroutine get_timespace_value_by_item

!> initialize_array_with_zero
      subroutine initialize_array_with_zero(array)

         real(kind=dp), allocatable, intent(inout) :: array(:) !< Array that will be initialized

         if (allocated(array)) then
            array(:) = 0.d0
         end if

      end subroutine initialize_array_with_zero

   end subroutine prepare_wind_model_data

!> Gets windstress (and air pressure) from input files, and sets the windstress
   subroutine calculate_wind_stresses(time_in_seconds, iresult)
      use m_wind, only: jawind, japatm
      use dfm_error, only: DFM_NOERR

      real(kind=dp), intent(in) :: time_in_seconds !< Current time when getting and applying winds
      integer, intent(out) :: iresult !< Error indicator
      if (jawind == 1 .or. japatm > 0) then
         call prepare_wind_model_data(time_in_seconds, iresult)
         if (iresult /= DFM_NOERR) then
            return
         end if
      end if

      if (jawind > 0) then
         call setwindstress()
      end if

      iresult = DFM_NOERR

   end subroutine calculate_wind_stresses

!> select_wave_variables_subgroup
!! select routine depending on whether all or a subgroup of wave variables are allocated
   subroutine select_wave_variables_subgroup(all_wave_variables)

      logical, intent(in) :: all_wave_variables

      if (all_wave_variables) then
         fill_open_boundary_cells_with_inner_values => fill_open_boundary_cells_with_inner_values_all
      else
         fill_open_boundary_cells_with_inner_values => fill_open_boundary_cells_with_inner_values_fewer
      end if

   end subroutine select_wave_variables_subgroup

!> fill_open_boundary_cells_with_inner_values_all
   subroutine fill_open_boundary_cells_with_inner_values_all(number_of_links, link2cell)
      use m_waves

      integer, intent(in) :: number_of_links !< number of links
      integer, intent(in) :: link2cell(:, :) !< indices of cells connected by links

      integer :: link !< link counter
      integer :: kb !< cell index of boundary cell
      integer :: ki !< cell index of internal cell

      do link = 1, number_of_links
         kb = link2cell(1, link)
         ki = link2cell(2, link)
         hwavcom(kb) = hwavcom(ki)
         twav(kb) = twav(ki)
         phiwav(kb) = phiwav(ki)
         uorbwav(kb) = uorbwav(ki)
         sxwav(kb) = sxwav(ki)
         sywav(kb) = sywav(ki)
         mxwav(kb) = mxwav(ki)
         mywav(kb) = mywav(ki)
         sbxwav(kb) = sbxwav(ki)
         sbywav(kb) = sbywav(ki)
         dsurf(kb) = dsurf(ki)
         dwcap(kb) = dwcap(ki)
      end do

   end subroutine fill_open_boundary_cells_with_inner_values_all

!> fill_open_boundary_cells_with_inner_values_fewer
   subroutine fill_open_boundary_cells_with_inner_values_fewer(number_of_links, link2cell)
      use m_waves
      use m_flowparameters, only: jawave, waveforcing

      integer, intent(in) :: number_of_links !< number of links
      integer, intent(in) :: link2cell(:, :) !< indices of cells connected by links

      integer :: link !< link counter
      integer :: kb !< cell index of boundary cell
      integer :: ki !< cell index of internal cell

      if (jawave == 7 .and. waveforcing == 2) then
         do link = 1, number_of_links
            kb = link2cell(1, link)
            ki = link2cell(2, link)
            distot(kb) = distot(ki)
         end do
      end if
      do link = 1, number_of_links
         kb = link2cell(1, link)
         ki = link2cell(2, link)
         hwavcom(kb) = hwavcom(ki)
         twav(kb) = twav(ki)
         phiwav(kb) = phiwav(ki)
         uorbwav(kb) = uorbwav(ki)
         sxwav(kb) = sxwav(ki)
         sywav(kb) = sywav(ki)
         mxwav(kb) = mxwav(ki)
         mywav(kb) = mywav(ki)
      end do

   end subroutine fill_open_boundary_cells_with_inner_values_fewer

   subroutine findexternalboundarypoints() ! find external boundary points
      use m_netw
      use m_flow, filetype_hide => filetype ! Two stages: 1 = collect elsets for which data is provided
      use m_flowgeom !             2 = add relations between elsets and their providers
      use unstruc_model ! This routine is based upon the network admin only,
      use timespace ! not on the flow admin.
      use m_sferic
      use m_alloc
      use m_ship
      use properties
      use m_transport
      use m_sobekdfm
      use m_sediment
      use m_partitioninfo
      use system_utils, only: split_filename
      use unstruc_files, only: resolvePath
      use m_qnerror
      use m_filez, only: oldfil, doclose

      implicit none

      character(len=256) :: filename
      integer :: filetype
      integer, allocatable :: kce(:) ! kc edges (numl)
      integer, allocatable :: ke(:) ! kc edges (numl)
      logical :: jawel
      integer :: ja_ext_force
      logical :: ext_force_bnd_used
      integer :: ierr, method
      real(kind=dp) :: return_time
      integer :: numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf
      integer :: nx
      integer :: ierror
      integer :: num_bc_ini_blocks
      character(len=64) :: varname

      jatimespace = 1

      return_time = 0
      ja_ext_force = 0
      ext_force_bnd_used = .false.

      if (len(trim(md_extfile)) > 0) then
         inquire (file=trim(md_extfile), exist=jawel)
         if (jawel) then
            if (mext /= 0) then
               ! Close first, if left open after prior flow_geominit().
               ! NOTE: AvD: this if-check relies on the fact that mext is *not* set to 0 in default_fm_external_forcing_data(), when reinitializing an already initialized model.
               call doclose(mext)
            end if

            call oldfil(mext, md_extfile)
            call split_filename(md_extfile, md_extfile_dir, filename) ! Remember base dir for this ext file
            ja_ext_force = 1
         else
            call qnerror('External forcing file '''//trim(md_extfile)//''' not found.', '  ', ' ')
            write (msgbuf, '(a,a,a)') 'External forcing file ''', trim(md_extfile), ''' not found.'
            call err_flush()
         end if
      end if
      if (len(trim(md_extfile_new)) > 0) then
         inquire (file=trim(md_extfile_new), exist=jawel)
         if (jawel) then
            ext_force_bnd_used = .true.
         else
            call qnerror('Boundary external forcing file '''//trim(md_extfile_new)//''' not found.', '  ', ' ')
            write (msgbuf, '(a,a,a)') 'Boundary external forcing file ''', trim(md_extfile_new), ''' not found.'
            call err_flush()
         end if
      end if

! if (ja_ext_force == 0 .and. .not. ext_force_bnd_used) then
!    return
! endif

      if (allocated(xe)) deallocate (xe, ye, xyen) ! centre points of all net links, also needed for opening closed boundaries

      !mx1Dend = 0                                        ! count MAX nr of 1D endpoints
      !do L = 1,numl1D
      !   if ( kn(3,L) == 1) then                         ! zeker weten
      !      k1 = kn(1,L) ; k2 = kn(2,L)
      !      if (nmk(k1) == 1 .and. nmk(k2) == 2 .and. lne(1,L) < 0 .or. &
      !          nmk(k2) == 1 .and. nmk(k1) == 2 .and. lne(2,L) < 0 ) then
      !          mx1Dend = mx1Dend + 1
      !      endif
      !   endif
      !enddo
      !
      !
      !nx = numl + mx1Dend

      ! count number of 2D links and 1D endpoints
      call count_links(mx1Dend, Nx)

      allocate (xe(nx), stat=ierr); xe = 0 ! used in findexternalboundarypoints
      call aerr('xe (nx)', ierr, nx)
      allocate (ye(nx), stat=ierr); ye = 0
      call aerr('ye (nx)', ierr, nx)
      allocate (xyen(2, nx), stat=ierr); xyen = 0d0
      call aerr('xyen(2, nx)', ierr, nx)

      ! some temp arrays

      if (allocated(kez)) then
         ! If flow_geominit was called separately from a flow_modelinit:
         deallocate (kez, keu, kes, ketm, kesd, keuxy, ket, ken, ke1d2d, keg, ked, kep, kedb, keklep, kevalv, kegs, kegen, itpez, itpenz, itpeu, itpenu, kew)
      end if
      if (allocated(ftpet)) then
         deallocate (ftpet)
      end if
      allocate (kce(nx), ke(nx), kez(nx), keu(nx), kes(nx), ketm(nx), kesd(nx), keuxy(nx), ket(nx), ken(nx), ke1d2d(nx), keg(nx), ked(nx), kep(nx), kedb(nx), keklep(nx), kevalv(nx), kegs(nx), kegen(nx), itpez(nx), itpenz(nx), itpeu(nx), itpenu(nx), kew(nx), ftpet(nx), stat=ierr)
      call aerr('kce(nx), ke(nx), kez(nx), keu(nx), kes(nx), ketm(nx), kesd(nx), keuxy(nx), ket(nx), ken(nx), ke1d2d(nx), keg(nx), ked(nx), kep(nx), kedb(nx), keklep(nx), kevalv(nx), kegs(nx), kegen(nx), itpez(nx), itpenz(nx), itpeu(nx) , itpenu(nx), kew(nx), ftpet(nx)', ierr, 17 * nx)
      kce = 0; ke = 0; kez = 0; keu = 0; kes = 0; ketm = 0; kesd = 0; keuxy = 0; ket = 0; ken = 0; ke1d2d = 0; keg = 0; ked = 0; kep = 0; kedb = 0; keklep = 0; kevalv = 0; kegen = 0; itpez = 0; itpenz = 0; itpeu = 0; itpenu = 0; kew = 0; ftpet = 1d6

      if (allocated(ketr)) deallocate (ketr)
      allocate (ketr(nx, 1), stat=ierr)
      call aerr('ketr(nx,1)', ierr, nx)
      ketr = 0

      if (allocated(nbndtr)) deallocate (nbndtr)
      allocate (nbndtr(1), stat=ierr)
      call aerr('nbndtr(1)', ierr, 1)
      nbndtr = 0

      if (allocated(trnames)) deallocate (trnames)
      allocate (trnames(1), stat=ierr)
      call aerr('trnames(1)', ierr, 1)
      trnames(1) = ''
      numtracers = 0

      if (allocated(kesf)) deallocate (kesf)
      allocate (kesf(1, nx), stat=ierr) ! would have been nice to have stmpar%lsedsus,
      call aerr('kesf(1,nx)', ierr, nx) ! but no can do, jammer de bammer...
      kesf = 0

      if (allocated(nbndsf)) deallocate (nbndsf)
      allocate (nbndsf(1), stat=ierr)
      call aerr('nbndsf(1)', ierr, 1)
      nbndsf = 0

      if (allocated(sfnames)) deallocate (sfnames)
      allocate (sfnames(1), stat=ierr)
      call aerr('sfnames(1)', ierr, 1)
      sfnames = ''
      numfracs = 0

      call make_mirrorcells(Nx, xe, ye, xyen, kce, ke, ierror)

      if (jampi == 1) then
! disable mirror cells that are not mirror cells in the whole model by setting kce=0
         call partition_reduce_mirrorcells(Nx, kce, ke, ierror)
      end if

      nbndz = 0 ! startindex waterlevel bnds
      nbndu = 0 ! startindex velocity   bnds
      nbnds = 0 ! startindex salinity   bnds
      nbndtm = 0 ! startindex temperature bnds
      nbndt = 0 ! startindex tangential vel. bnds
      nbnduxy = 0 ! startindex uxuy vel. bnds
      nbndn = 0 ! startindex normal     vel. bnds
      nbnd1d2d = 0 ! startindex 1d2d bnds
      ngate = 0 ! startindex gate links
      ncdam = 0 ! startindex cdam links
      npump = 0 ! startindex pump links
      nbndw = 0 ! startindex wave energy bnds

      nqbnd = 0 ! nr of q sections   or specified q bnd's
      nqhbnd = 0 ! nr of qh boundary sections or specified qh bnd's
      ngatesg = 0 ! nr of gate signals or specified gates ! not in loop below because flow links not ready yet
      ncdamsg = 0 ! nr of controllable dam signals
      npumpsg = 0 ! nr of pump signals
      nshiptxy = 0 ! nr of ship xyt signals
      nwbnd = 0 ! nr of wave-energy boundaries

      num_bc_ini_blocks = 0
      if (ext_force_bnd_used) then
         ! first read the bc file (new file format for boundary conditions)
         call read_location_files_from_boundary_blocks(trim(md_extfile_new), nx, kce, num_bc_ini_blocks, &
                                                       numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf)
      end if

      do while (ja_ext_force == 1) ! read *.ext file

         call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja_ext_force, varname)
         call resolvePath(filename, md_extfile_dir)

         if (num_bc_ini_blocks > 0 .and. qid(len_trim(qid) - 2:len_trim(qid)) == 'bnd') then
            write (msgbuf, '(a)') 'Boundaries in BOTH external forcing and bound.ext.force file is not allowed'
            call msg_flush()
            call qnerror('Boundaries in two files: ', trim(md_extfile_new), ' and '//trim(md_extfile))
            ja_ext_force = 0
         end if

         if (ja_ext_force == 1) then

            jatimespace = 1 ! module is to be used

            call processexternalboundarypoints(qid, filename, filetype, return_time, nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, 1d0, transformcoef)

         end if

      end do

      deallocate (kce)
      deallocate (ke)

      if (mext /= 0) then
         rewind (mext) ! prepare input file
      end if
      numbnp = nbndz + nbndu + nbnd1d2d ! nr of boundary points =

   end subroutine findexternalboundarypoints

   subroutine read_location_files_from_boundary_blocks(filename, nx, kce, num_bc_ini_blocks, &
                                                       numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf)
      use properties
      use timespace, only: NODE_ID, POLY_TIM
      use tree_data_types
      use tree_structures
      use m_flowgeom, only: rrtol
      use fm_external_forcings_data, only: transformcoef
      use system_utils
      use unstruc_files, only: resolvePath
      use m_alloc
      use string_module, only: strcmpi
      use unstruc_model, only: ExtfileNewMajorVersion, ExtfileNewMinorVersion
      use m_missing, only: dmiss
      use m_qnerror

      implicit none

      character(len=*), intent(in) :: filename
      integer, intent(in) :: nx
      integer, dimension(nx), intent(inout) :: kce
      integer, intent(out) :: num_bc_ini_blocks
      integer, intent(inout) :: numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf

      type(tree_data), pointer :: bnd_ptr !< tree of extForceBnd-file's [boundary] blocks
      type(tree_data), pointer :: node_ptr !
      integer :: filetype !< possible values POLY_TIM: use polygon file as location reference, or NODE_ID: use nodeId as a location reference
      integer :: istat !
      integer, parameter :: ini_key_len = 32 !
      integer, parameter :: ini_value_len = 256 !
      character(len=ini_key_len) :: groupname !
      character(len=ini_value_len) :: quantity !
      character(len=ini_value_len) :: location_file !< contains either the name of the polygon file (.pli) or the nodeId
      character(len=ini_value_len) :: forcing_file !
      real(kind=dp) :: return_time !
      real(kind=dp) :: tr_ws ! Tracer fall velocity
      real(kind=dp) :: tr_decay_time ! Tracer decay time
      real(kind=dp) :: rrtolb ! Local, optional boundary tolerance value.
      real(kind=dp) :: width1D ! Local, optional custom 1D boundary width
      real(kind=dp) :: blDepth ! Local, optional custom boundary bed level depth below initial water level

      integer :: i
      integer :: num_items_in_file
      logical :: file_ok
      logical :: group_ok
      logical :: property_ok
      character(len=256) :: basedir, fnam
      integer :: major, minor

      call tree_create(trim(filename), bnd_ptr)
      call prop_file('ini', trim(filename), bnd_ptr, istat)
      if (istat /= 0) then
         call qnerror('Boundary external forcing file ', trim(filename), ' could not be read')
         return
      end if

      ! check FileVersion
      major = 1
      minor = 0
      call get_version_number(bnd_ptr, major=major, minor=minor, success=file_ok)
      if ((major /= ExtfileNewMajorVersion .and. major /= 1) .or. minor > ExtfileNewMinorVersion) then
         write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of new external forcing file detected in '''//trim(filename)//''': v', major, minor, '. Current format: v', ExtfileNewMajorVersion, ExtfileNewMinorVersion, '. Ignoring this file.'
         call err_flush()
         return
      end if

      call split_filename(filename, basedir, fnam) ! Remember base dir of input file, to resolve all refenced files below w.r.t. that base dir.

      num_items_in_file = 0
      if (associated(bnd_ptr%child_nodes)) then
         num_items_in_file = size(bnd_ptr%child_nodes)
      end if

      file_ok = .true.
      do i = 1, num_items_in_file
         node_ptr => bnd_ptr%child_nodes(i)%node_ptr
         groupname = tree_get_name(bnd_ptr%child_nodes(i)%node_ptr)
         if (strcmpi(groupname, 'Boundary')) then
            quantity = ''
            location_file = ''
            forcing_file = ''
            return_time = 0.0

            group_ok = .true.

            ! todo: read multiple quantities
            call prop_get(node_ptr, '', 'quantity', quantity, property_ok)
            if (.not. property_ok) then
               call qnerror('Expected property', 'quantity', 'for boundary definition')
            end if

            group_ok = group_ok .and. property_ok

            call prop_get(node_ptr, '', 'nodeId', location_file, property_ok)
            if (property_ok) then
               filetype = NODE_ID
            else
               call prop_get(node_ptr, '', 'locationFile', location_file, property_ok)
               filetype = POLY_TIM
            end if

            if (property_ok) then
               call resolvePath(location_file, basedir)
            else
               call qnerror('Expected property', 'locationFile', 'for boundary definition')
            end if

            group_ok = group_ok .and. property_ok

            call prop_get(node_ptr, '', 'forcingFile ', forcing_file, property_ok)
            if (property_ok) then
               call resolvePath(forcing_file, basedir)
            else
               call qnerror('Expected property', 'forcingFile', 'for boundary definition')
            end if

            group_ok = group_ok .and. property_ok

            call prop_get(node_ptr, '', 'returnTime', return_time)
            call prop_get(node_ptr, '', 'return_time', return_time) ! UNST-2386: Backwards compatibility reading.

            tr_ws = 0d0
            call prop_get(node_ptr, '', 'tracerFallVelocity', tr_ws)
            transformcoef(4) = tr_ws

            tr_decay_time = 0d0
            call prop_get(node_ptr, '', 'tracerDecayTime', tr_decay_time)
            transformcoef(5) = tr_decay_time

            rrtolb = 0d0
            call prop_get(node_ptr, '', 'openBoundaryTolerance', rrtolb)

            width1D = dmiss
            call prop_get(node_ptr, '', 'bndWidth1D', width1D)

            blDepth = dmiss
            call prop_get(node_ptr, '', 'bndBlDepth', blDepth)

            if (group_ok) then
               if (rrtolb > 0d0) then
                  call processexternalboundarypoints(quantity, location_file, filetype, return_time, nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, rrtolrel=(1 + 2 * rrtolb) / (1 + 2 * rrtol), tfc=transformcoef, width1D=width1D, blDepth=blDepth)
               else
                  call processexternalboundarypoints(quantity, location_file, filetype, return_time, nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, rrtolrel=1d0, tfc=transformcoef, width1D=width1D, blDepth=blDepth)
               end if
               num_bc_ini_blocks = num_bc_ini_blocks + 1
            end if

            file_ok = file_ok .and. group_ok

         else
            ! warning: unknown group
         end if

      end do

      call tree_destroy(bnd_ptr)

   end subroutine read_location_files_from_boundary_blocks

   subroutine appendrettime(qidfm, nbnd, rettime)

      use fm_external_forcings_data
      use m_alloc

      implicit none

      character(len=256), intent(in) :: qidfm ! constituent index
      integer, intent(in) :: nbnd ! boundary cell index
      real(kind=dp), intent(in) :: rettime ! return time (h)
      integer :: thrtlen ! temp array length

      if (allocated(thrtt)) then
         thrtlen = size(thrtt) + 1
      else
         thrtlen = 1
      end if

      call realloc(thrtq, thrtlen, keepExisting=.true., fill='')
      thrtq(thrtlen) = qidfm

      call realloc(thrtn, thrtlen, keepExisting=.true., fill=0)
      thrtn(thrtlen) = nbnd

      call realloc(thrtt, thrtlen, keepExisting=.true., fill=0d0)
      thrtt(thrtlen) = rettime
   end subroutine appendrettime

   !> helper routine finding external boundary points, called for both old and new-type ext file.
   !! Also used for some none-boundary quantities that also need counting total nr of elements, *prior* to flow_initexternalforcings.
   !! Two stages: 1 = collect elsets for which data is provided         <-- findexternalboundarypoints + processexternalboundarypoints
   !!             2 = add relations between elsets and their providers  <-- flow_initexternalforcings
   !! This routine is based upon the network admin only, not on the flow admin.
   subroutine processexternalboundarypoints(qid, filename, filetype, return_time, nx, kce, &
                                            numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, &
                                            numqh, numw, numtr, numsf, rrtolrel, tfc, &
                                            width1D, blDepth) ! helper for finding external boundary points
      use m_netw
      use m_flow, qid_flow => qid, filetype_flow => filetype
      use m_flowgeom
      use unstruc_model
      use timespace
      use m_sferic
      use m_alloc
      use m_ship
      use properties
      use m_transport
      use m_meteo, qid_meteo => qid, filetype_meteo => filetype
      use m_sobekdfm
      use m_flowparameters, only: jawave
      use string_module
      use m_strucs, only: NUMGENERALKEYWRD
      use m_missing, only: dmiss
      use m_qnerror
      use m_find_name, only: find_name

      implicit none

      character(len=256), intent(in) :: qid !
      character(len=256), intent(in) :: filename !
      integer, intent(in) :: filetype
      integer, intent(in) :: nx !
      integer, dimension(nx), intent(inout) :: kce !
      real(kind=dp), intent(in) :: return_time
      integer, intent(inout) :: numz, numu, nums, numtm, numsd, & !
                                numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf !
      real(kind=dp), intent(in) :: rrtolrel !< To enable a more strict rrtolerance value than the global rrtol. Measured w.r.t. global rrtol.

      real(kind=dp), dimension(NUMGENERALKEYWRD), optional, intent(in) :: tfc
      real(kind=dp), optional, intent(in) :: width1D !< Optional custom width for boundary flow link.
      real(kind=dp), optional, intent(in) :: blDepth !< Optional custom bed level depths below water level boundaries's initial value for boundary points.

      character(len=256) :: qidfm !
      integer :: itpbn
      character(len=NAMTRACLEN) :: tracnam, sfnam, qidnam
      character(len=20) :: tracunit
      integer :: itrac, isf
      integer :: janew
      character(len=:), allocatable :: pliname

! call bndname_to_fm(qid,qidfm)
      qidfm = qid
      if (qidfm == 'waterlevelbnd' .or. qidfm == 'neumannbnd' .or. qidfm == 'riemannbnd' .or. qidfm == 'outflowbnd' .or. qidfm == 'qhbnd') then

         if (allocated(pliname)) deallocate (pliname)
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, kez(nbndz + 1:nx), numz, usemask=.true., pliname=pliname) !numz=number cells found, plname=pliname
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numz, ' nr of open bndcells'; call msg_flush()
         nzbnd = nzbnd + 1

         if (qidfm == 'waterlevelbnd') itpbn = 1
         if (qidfm == 'neumannbnd') itpbn = 2
         if (qidfm == 'riemannbnd') then
            itpbn = 5
            if (present(tfc)) then
               ftpet(nbndz + 1:nbndz + numz) = tfc(7) ! relaxation time riemann from ext file
            end if
         end if
         if (qidfm == 'outflowbnd') itpbn = 6

         if (qidfm == 'qhbnd') then
            itpbn = 7
            nqhbnd = nqhbnd + 1
            numqh = numz
            if (filetype == poly_tim) then
               call realloc(qhpliname, nqhbnd); qhpliname(nqhbnd) = pliname
            end if

            call realloc(L1qhbnd, nqhbnd); L1qhbnd(nqhbnd) = nbndz + 1
            call realloc(L2qhbnd, nqhbnd); L2qhbnd(nqhbnd) = nbndz + numz
            call realloc(atqh_all, nqhbnd); atqh_all(nqhbnd) = 0d0
            call realloc(atqh_sum, nqhbnd); atqh_sum(nqhbnd) = 0d0
            call realloc(qhbndz, nqhbnd); qhbndz(nqhbnd) = 0d0
            call realloc(qh_gamma, nqhbnd)
            qh_gamma = 0d0
            call realloc(qhbndz_min, nqhbnd)
            qhbndz_min = 0d0
            call realloc(qhbndz_plus, nqhbnd)
            qhbndz_plus = 0d0
            call realloc(q_org, nqhbnd)
            q_org = 0d0
         end if
         itpez(nbndz + 1:nbndz + numz) = itpbn

         call addopenbndsection(numz, kez(nbndz + 1:nbndz + numz), filename, IBNDTP_ZETA)

         ! When present, set custom geometry for open boundaries (bed level for bndz and/or width1D for 1D bndz/u).
         ! Only for z:
         if (present(blDepth)) then
            call realloc(bndBlDepth, size(openbndtype), fill=dmiss)
            bndBlDepth(nopenbndsect) = blDepth
         end if
         ! For z and u:
         if (present(width1D)) then
            call realloc(bndWidth1D, size(openbndtype), fill=dmiss)
            bndWidth1D(nopenbndsect) = width1D
         end if

         itpenz(nbndz + 1:nbndz + numz) = nopenbndsect
         nbndz = nbndz + numz

      else if (qidfm == 'velocitybnd' .or. qidfm == 'dischargebnd' .or. qidfm == 'qhubnd' .or. &
               qidfm == 'criticaloutflowbnd' .or. qidfm == 'weiroutflowbnd' .or. qidfm == 'absgenbnd') then
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, keu(nbndu + 1:nx), numu, usemask=.true., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numu, ' nr of open bndcells'; call msg_flush()
         nubnd = nubnd + 1

         if (qidfm == 'velocitybnd') then
            itpbn = 3
         else if (qidfm == 'dischargebnd') then
            itpbn = 4
            nqbnd = nqbnd + 1
            call realloc(L1qbnd, nqbnd); L1qbnd(nqbnd) = nbndu + 1
            call realloc(L2qbnd, nqbnd); L2qbnd(nqbnd) = nbndu + numu
            call realloc(at_all, nqbnd); at_all(nqbnd) = 0d0
            call realloc(at_sum, nqbnd); at_sum(nqbnd) = 0d0
            call realloc(wwssav_all, (/2, nqbnd/), keepExisting=.true., fill=0d0)
            call realloc(wwssav_sum, (/2, nqbnd/), keepExisting=.true., fill=0d0)
            call realloc(huqbnd, L2qbnd(nqbnd)); huqbnd(L1qbnd(nqbnd):L2qbnd(nqbnd)) = 0d0
         else if (qidfm == 'absgenbnd') then
            if (.not. (jawave == 4)) then ! Safety to avoid allocation errors later on
               call qnerror('Absorbing-generating boundary defined without activating surfbeat model. Please use appropriate wave model, or change the boundary condition type.', '  ', ' ')
               write (msgbuf, '(a)') 'Absorbing-generating boundary defined without activating surfbeat model. Please use appropriate wave model, or change the boundary condition type.'
               call err_flush()
            end if
            itpbn = 5
         else if (qidfm == 'qhubnd') then
            itpbn = 6
         else if (qidfm == 'criticaloutflowbnd') then
            itpbn = 8
         else if (qidfm == 'weiroutflowbnd') then
            itpbn = 9
         end if

         itpeu(nbndu + 1:nbndu + numu) = itpbn

         call addopenbndsection(numu, keu(nbndu + 1:nbndu + numu), filename, IBNDTP_U)

         ! When present, set custom geometry for open boundaries (width1D for 1D bndz/u).
         ! For z and u:
         if (present(width1D)) then
            call realloc(bndWidth1D, size(openbndtype), fill=dmiss)
            bndWidth1D(nopenbndsect) = width1D
         end if

         itpenu(nbndu + 1:nbndu + numu) = nopenbndsect
         nbndu = nbndu + numu

      else if (qidfm == 'salinitybnd' .and. jasal > 0) then
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, kes(nbnds + 1:nx), nums, usemask=.false., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), nums, ' nr of salinity bndcells'; call msg_flush()
         if (nums > 0) then
            call appendrettime(qidfm, nbnds + 1, return_time)
            nbnds = nbnds + nums
         end if

      else if (qidfm == 'waveenergybnd') then
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, kew(nbndw + 1:nx), numw, usemask=.false., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numw, ' nr of wave energy bndcells'; call msg_flush()
         nwbnd = nwbnd + 1

         call realloc(L1wbnd, nwbnd); L1wbnd(nwbnd) = nbndw + 1
         call realloc(L2wbnd, nwbnd); L2wbnd(nwbnd) = nbndw + numw

         nbndw = nbndw + numw
         call realloc(fnamwbnd, nwbnd, fill='')
         fnamwbnd(nwbnd) = trim(filename)

      else if (qidfm == 'temperaturebnd' .and. jatem > 0) then
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, ketm(nbndtm + 1:nx), numtm, usemask=.false., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numtm, ' nr of temperature bndcells'; call msg_flush()
         if (numtm > 0) then
            call appendrettime(qidfm, nbndtm + 1, return_time)
            nbndtm = nbndtm + numtm
         end if

      else if (qidfm == 'sedimentbnd') then
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, kesd(nbndsd + 1:nx), numsd, usemask=.false., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numsd, ' nr of sediment bndcells'; call msg_flush()
         if (numsd > 0) then
            call appendrettime(qidfm, nbndsd + 1, return_time)
            nbndsd = nbndsd + numsd
         end if

      else if (qidfm(1:9) == 'tracerbnd') then
         call get_tracername(qidfm, tracnam, qidnam)
         tracunit = " "
         call add_bndtracer(tracnam, tracunit, itrac, janew)

         if (janew == 1) then
!       realloc ketr
            call realloc(ketr, (/Nx, numtracers/), keepExisting=.true., fill=0)
         end if
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, ketr(nbndtr(itrac) + 1:, itrac), numtr, usemask=.false., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numtr, ' nr of tracer bndcells'; call msg_flush()
         if (numtr > 0) then
            call appendrettime(qidfm, nbndtr(itrac) + 1, return_time)
            nbndtr(itrac) = nbndtr(itrac) + numtr
            nbndtr_all = maxval(nbndtr(1:numtracers))
         end if

      else if (qid(1:13) == 'initialtracer') then
         call get_tracername(qid, tracnam, qidnam)
         tracunit = " "
         call add_bndtracer(tracnam, tracunit, itrac, janew)

         if (janew == 1) then
!       realloc ketr
            call realloc(ketr, (/Nx, numtracers/), keepExisting=.true., fill=0)
         end if

      else if (qidfm(1:10) == 'sedfracbnd' .and. stm_included) then
         call get_sedfracname(qidfm, sfnam, qidnam)
         isf = find_name(sfnames, sfnam)

         if (isf == 0) then ! add

            numfracs = numfracs + 1
!       realloc
            call realloc(kesf, (/Nx, numfracs/), keepExisting=.true., fill=0)
            call realloc(nbndsf, numfracs, keepExisting=.true., fill=0)
            call realloc(sfnames, numfracs, keepExisting=.true., fill='')

            sfnames(numfracs) = trim(sfnam)
            isf = numfracs

         end if

         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, kesf(nbndsf(isf) + 1:, isf), numsf, usemask=.false., rrtolrel=rrtolrel)
         write (msgbuf, '(3a,i8,a)') trim(qid), ' ', trim(filename), numsf, ' nr of sedfrac bndcells'; call msg_flush()
         if (numsf > 0) then
            call appendrettime(qidfm, nbndsf(isf) + 1, return_time)
            nbndsf(isf) = nbndsf(isf) + numsf
            nbndsf_all = maxval(nbndsf(1:numfracs))
         end if

      else if (qidfm == 'tangentialvelocitybnd') then
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, ket(nbndt + 1:nx), numt, usemask=.false., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numt, ' nr of tangentialvelocity bndcells'; call msg_flush()

         nbndt = nbndt + numt

      else if (qidfm == 'uxuyadvectionvelocitybnd') then
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, keuxy(nbnduxy + 1:nx), numuxy, usemask=.false., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numuxy, ' nr of uxuyadvectionvelocity bndcells'; call msg_flush()

         nbnduxy = nbnduxy + numuxy

      else if (qidfm == 'normalvelocitybnd') then
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, ken(nbndn + 1:nx), numn, usemask=.false., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numn, ' nr of normalvelocity bndcells'; call msg_flush()

         nbndn = nbndn + numn

      else if (qidfm == '1d2dbnd') then ! SOBEK1D-FM2D
         call selectelset(filename, filetype, xe, ye, xyen, kce, nx, ke1d2d(nbnd1d2d + 1:nx), num1d2d, usemask=.true., rrtolrel=rrtolrel)
         write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), num1d2d, ' nr of SOBEK1D-FM2D bndcells'; call msg_flush()

         call addopenbndsection(num1d2d, ke1d2d(nbnd1d2d + 1:nbnd1d2d + num1d2d), filename, IBNDTP_1D2D)
         nbnd1d2d = nbnd1d2d + num1d2d

      else if (qidfm == 'shiptxy') then

         nshiptxy = nshiptxy + 1

      else if (qidfm == 'nudgetime' .or. qidfm == 'nudgerate' .or. qidfm == 'nudge_salinity_temperature') then

         janudge = 1

      end if

   end subroutine processexternalboundarypoints

!> Calls the ec_addtimespacerelation with all proper unstruc-specific target arrays and element set masks.
   function addtimespacerelation_boundaries(qid, filename, filetype, method, operand, forcing_file, targetindex) result(success)
      use fm_external_forcings_data, no1 => qid, no2 => filetype, no3 => operand, no4 => success
      use m_meteo, no5 => qid, no6 => filetype, no7 => operand, no8 => success
      use m_flowparameters, only: jawave
      use m_flowtimes, only: dt_nodal
      use m_qnerror
      use m_find_name, only: find_name

      implicit none

      character(len=*), intent(inout) :: qid !< Identifier of current quantity (i.e., 'waterlevelbnd')
      character(len=*), intent(in) :: filename !< Name of data file for current quantity.
      integer, intent(in) :: filetype !< File type of current quantity.
      integer, intent(in) :: method !< Time-interpolation method for current quantity.
      character(len=1), intent(in) :: operand !< Operand w.r.t. previous data ('O'verride or '+'Append)
      character(len=*), optional, intent(in) :: forcing_file !< Optional forcings file, if it differs from the filename (i.e., if filename=*.pli, and forcing_file=*.bc)
      integer, optional, intent(in) :: targetIndex !< target position or rank of (complete!) vector in target array

      logical :: success
      character(len=256) :: tracnam, sfnam, qidnam
      integer :: itrac, isf
      real(kind=dp), dimension(:), pointer :: pzmin, pzmax

      success = .true. ! initialization

      ! Special forcingsfile: if name equals 'REALTIME', do not do an ec_addtimespacerelation, just leave it to the external caller to fill zbnd* target value array.
      ! TODO: AVD: we now leave it to caller to fill array with length(zbnd*),
      ! instead of the number of polyline points. Cleaner alternative is to create
      ! a poly_tim provider, with the *underlying* point child providers being REALTIME.
      if (present(forcing_file)) then
         if (trim(forcing_file) == 'REALTIME') then
            call mess(LEVEL_DEBUG, 'addtimespacerelation_boundaries: leave empty timespacerelation for '''//trim(qid)//''' from locationFile '''//trim(filename)//''' (REALTIME data).')
            return
         end if
      end if

      kx = 1
      if (nbndz > 0 .and. (qid == 'waterlevelbnd' .or. qid == 'neumannbnd' .or. qid == 'riemannbnd' .or. qid == 'outflowbnd')) then
         success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcing_file, dtnodal=dt_nodal, targetindex=targetindex)

      else if (nbndz > 0 .and. nqhbnd > 0 .and. (qid == 'qhbnd')) then
         success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbndu > 0 .and. (qid == 'dischargebnd' .or. qid == 'criticaloutflowbnd' .or. qid == 'weiroutflowbnd' .or. qid == 'absgenbnd')) then
         if (qid == 'absgenbnd') then
            jawave = 4
         end if
         success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand, xy2bndu, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbndu > 0 .and. qid == 'velocitybnd') then
         pzmin => zminmaxu(1:nbndu)
         pzmax => zminmaxu(nbndu + 1:2 * nbndu)
         success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand, &
                                           xy2bndu, z=sigmabndu, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbnds > 0 .and. qid == 'salinitybnd') then ! 2D
         pzmin => zminmaxs(1:nbnds)
         pzmax => zminmaxs(nbnds + 1:2 * nbnds)
         success = ec_addtimespacerelation(qid, xbnds, ybnds, kds, kx, filename, filetype, method, operand, xy2bnds, &
                                           z=sigmabnds, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbndTM > 0 .and. qid == 'temperaturebnd') then
         pzmin => zminmaxtm(1:nbndTM)
         pzmax => zminmaxtm(nbndTM + 1:2 * nbndTM)
         success = ec_addtimespacerelation(qid, xbndTM, ybndTM, kdtm, kx, filename, filetype, method, operand, xy2bndtm, &
                                           z=sigmabndtm, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbndsd > 0 .and. (qid == 'sedimentbnd')) then
         pzmin => zminmaxsd(1:nbndsd)
         pzmax => zminmaxsd(nbndsd + 1:2 * nbndsd)
         success = ec_addtimespacerelation(qid, xbndsd, ybndsd, kdsd, kx, filename, filetype, method, operand, xy2bndsd, &
                                           z=sigmabndsd, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file, targetindex=targetindex)

      else if (numtracers > 0 .and. (qid(1:9) == 'tracerbnd')) then
         ! get tracer boundary condition number
         call get_tracername(qid, tracnam, qidnam)
         itrac = find_name(trnames, tracnam)

! for parallel runs, we always need to add the tracer, even if this subdomain has no tracer boundary conditions defined
!      call add_tracer(tracnam, iconst)
!      update: all tracers are counted first and allocated later

         if (nbndtr(itrac) > 0) then
            pzmin => bndtr(itrac)%zminmax(1:nbndtr(itrac))
            pzmax => bndtr(itrac)%zminmax(nbndtr(itrac) + 1:2 * nbndtr(itrac))
            success = ec_addtimespacerelation(qid, bndtr(itrac)%x, bndtr(itrac)%y, bndtr(itrac)%kd, kx, filename, filetype, method, operand, bndtr(itrac)%xy2, &
                                              z=bndtr(itrac)%sigma, forcingfile=forcing_file, pzmin=pzmin, pzmax=pzmax, targetindex=targetindex)
         else
            success = .true.
         end if

      else if (numfracs > 0 .and. (qid(1:10) == 'sedfracbnd') .and. stm_included) then

         call get_sedfracname(qid, sfnam, qidnam)
         isf = find_name(sfnames, sfnam)

         if (isf > 0) then
            if (nbndsf(isf) > 0) then
               pzmin => bndsf(isf)%zminmax(1:nbndsf(isf))
               pzmax => bndsf(isf)%zminmax(nbndsf(isf) + 1:2 * nbndsf(isf))
               success = ec_addtimespacerelation(qid, bndsf(isf)%x, bndsf(isf)%y, bndsf(isf)%kd, kx, filename, filetype, method, operand, bndsf(isf)%xy2, &
                                                 z=bndsf(isf)%sigma, forcingfile=forcing_file, pzmin=pzmin, pzmax=pzmax, targetindex=targetindex)
            else
               success = .true.
            end if
         else
            call mess(LEVEL_WARN, 'Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.')
            call qnerror('Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.', ' ', ' ')
         end if

      else if (nbndt > 0 .and. (qid == 'tangentialvelocitybnd')) then
         success = ec_addtimespacerelation(qid, xbndt, ybndt, kdt, kx, filename, filetype, method, operand, xy2bndt, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbnduxy > 0 .and. (qid == 'uxuyadvectionvelocitybnd')) then
         kx = 2
         pzmin => zminmaxuxy(1:nbnduxy)
         pzmax => zminmaxuxy(nbnduxy + 1:2 * nbnduxy)
         success = ec_addtimespacerelation(qid, xbnduxy, ybnduxy, kduxy, kx, filename, filetype, method, operand, xy2bnduxy, &
                                           z=sigmabnduxy, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file)

      else if (nbndn > 0 .and. (qid == 'normalvelocitybnd')) then
         success = ec_addtimespacerelation(qid, xbndn, ybndn, kdn, kx, filename, filetype, method, operand, xy2bndn, forcingfile=forcing_file, targetindex=targetindex)

      else !There is some boundary that is not detected or recognized
!      success = .false.
! SPvdP: this is not an error, especially for parallel runs
      end if
   end function addtimespacerelation_boundaries

!> Initializes memory for laterals on flow nodes.
   subroutine ini_alloc_laterals()
      use m_laterals, only: kclat, nnlat
      use m_flowgeom, only: ndx2d, ndxi, ndx
      use m_alloc
      integer :: ierr
      integer :: nlatndguess

      if (.not. allocated(nnlat)) then ! just once
         nlatndguess = ndx2d + 2 * (ndxi - ndx2d) ! first guess: all 2D + twice all 1D, nnlat *might* be bigger.
         allocate (nnLat(nlatndguess), stat=ierr)
         call aerr('nnLat(nlatndguess)', ierr, nlatndguess)
         nnLat = 0
      end if
      if (.not. allocated(kcLat)) then
         allocate (kcLat(ndx), stat=ierr) ! only if needed
         call aerr('kcLat(ndx)', ierr, ndx)
      end if
   end subroutine ini_alloc_laterals

!> Calls the ec_addtimespacerelation with all proper dflowfm-specific
!! target arrays and element set masks for object parameters with
!! spatially uniform time series.
!! Also handles inside one function the old-style *.ext quantities and
!! the new style *.ext and structures.ini quantities.
   function adduniformtimerelation_objects(qid, location_file, objtype, objid, paramname, paramvalue, targetindex, vectormax, targetarray) result(success)
      !use fm_external_forcings_data, no1=>qid, no2=>filetype, no3=>operand, no4 => success
      use m_meteo, no5 => qid, no6 => filetype, no7 => operand, no8 => success
      use string_module, only: strcmpi
      use timespace_parameters, only: uniform, bcascii, spaceandtime

      implicit none

      character(len=*), intent(in) :: qid !< Identifier of current quantity (i.e., 'waterlevelbnd')
      character(len=*), intent(in) :: location_file !< Name of location file (*.pli or *.pol) for current quantity (leave empty when valuestring contains value or filename).
      character(len=*), intent(in) :: objtype !< Type name of the object for which this relation is set (e.g., 'lateral', for prettyprinting only).
      character(len=*), intent(in) :: objid !< Id of the object for which this relation is set (for prettyprinting only).
      character(len=*), intent(in) :: paramname !< Name of the parameter that is set in this relation (e.g., 'discharge', for prettyprinting only).
      character(len=*), intent(in) :: paramvalue !< String containing the parameter value (either a scalar double, or 'REALTIME', or a filename)
      integer, intent(in) :: targetindex !< Target index in target value array (typically, the current count of this object type, e.g. numlatsg).
      integer, intent(in) :: vectormax !< The number of values per object ('kx'), typically 1.
      logical :: success !< Return value. Whether relation was added successfully.
      real(kind=dp), intent(inout), target :: targetarray(:) !< The target array in which the value(s) will be stored. Either now with scalar, or later via ec_gettimespacevalue() calls.

      character(len=256) :: valuestring, fnam
      real(kind=dp) :: valuedble
      real(kind=dp) :: xdum(1), ydum(1)
      integer :: kdum(1)
      integer :: ierr, L
      real(kind=dp), pointer :: targetarrayptr(:)
      real(kind=dp), pointer :: dbleptr(:)
      integer :: tgtitem
      integer, pointer :: intptr, multuniptr
      logical :: file_exists

      success = .true. ! initialization
      xdum = 1d0; ydum = 1d0; kdum = 1

      if (len_trim(paramvalue) > 0) then
         valuestring = paramvalue
      else if (len_trim(location_file) > 0) then
         ! Old-style *.ext:
         ! Prepare time series relation, if the .pli file has an associated .tim file.
         L = index(location_file, '.', back=.true.) - 1
         valuestring = location_file(1:L)//'_0001.tim'
         inquire (file=valuestring, exist=file_exists)
         if (.not. file_exists) then
            valuestring = location_file(1:L)//'.tim'
            inquire (file=valuestring, exist=file_exists)
            if (.not. file_exists) then
               call mess(LEVEL_ERROR, 'Files '''//trim(valuestring)//''' and file '''//trim(location_file(1:L)//'_0001.tim')//''' do not exist.')
               success = .false.
               return
            end if
         end if

      else
         call mess(LEVEL_ERROR, trim(objtype)//' '''//trim(objid)//''', '//paramname//' could not be read.')
         success = .false.
      end if

      ! Now check the valuestring for either scalar/REALTIME/.tim filename
      read (valuestring, *, iostat=ierr) valuedble
      targetarrayptr => targetarray
      tgtitem = ec_undef_int

      if (ierr /= 0 .or. index(valuestring, '/') == 1) then ! No number or a string starting with '/': check for timeseries filename
         if (strcmpi(trim(valuestring), 'REALTIME')) then
            success = .true.
            ! targetarray(targetindex) should be filled via DLL's API
            write (msgbuf, '(a,a,a,a,a)') 'Control for ', trim(objtype), ''''//trim(objid)//''', ', paramname, ' set to REALTIME.'
            call dbg_flush()
         else
            if (fm_ext_force_name_to_ec_item('', '', '', qid, multuniptr, intptr, intptr, intptr, dbleptr, dbleptr, dbleptr, dbleptr)) then
               success = .true.
            else
               success = .false.
               write (msgbuf, '(a)') 'Unknown quantity '''//trim(qid)//'''.'
               call err_flush()
               return
            end if

            fnam = trim(valuestring)
            ! Time-interpolated value will be placed in target array (e.g., qplat(n)) when calling ec_gettimespacevalue.
            if (index(trim(fnam)//'|', '.tim|') > 0) then
               ! uniform=single time series vectormax = 1
               success = ec_addtimespacerelation(qid, xdum, ydum, kdum, vectormax, fnam, &
                                                 filetype=uniform, &
                                                 method=spaceandtime, &
                                                 operand='O', &
                                                 tgt_data1=targetarrayptr, &
                                                 tgt_item1=tgtitem, &
                                                 multuni1=multuniptr, &
                                                 targetIndex=targetindex)
            elseif (index(trim(fnam)//'|', '.bc|') > 0) then
               ! uniform=single time series vectormax = 1

               success = ec_addtimespacerelation(qid, xdum, ydum, kdum, vectormax, objid, &
                                                 filetype=bcascii, &
                                                 method=spaceandtime, &
                                                 operand='O', &
                                                 tgt_data1=targetarrayptr, &
                                                 tgt_item1=tgtitem, &
                                                 multuni1=multuniptr, &
                                                 targetIndex=targetindex, &
                                                 forcingFile=fnam)
            end if
         end if
      else
         targetarray(targetindex) = valuedble ! Constant value for always, set it now already.
      end if
   end function adduniformtimerelation_objects

   subroutine register_quantity_pli_combination(quantity, location_file)
      use m_alloc
      implicit none
      character(len=*), intent(in) :: quantity
      character(len=*), intent(in) :: location_file
      character(len=max_registered_item_id) :: item_id

      item_id = trim(quantity)//'-'//trim(location_file)

      if (num_registered_items >= max_ext_bnd_items) then
         max_ext_bnd_items = ceiling(1.2 * num_registered_items)
         call realloc(registered_items, max_ext_bnd_items, keepExisting=.true., fill='')
      end if

      num_registered_items = num_registered_items + 1
      registered_items(num_registered_items) = item_id

   end subroutine register_quantity_pli_combination

   subroutine init_registered_items()
      implicit none
      num_registered_items = 0

      max_ext_bnd_items = 64 ! Default start size.
      if (allocated(registered_items)) deallocate (registered_items)
      allocate (registered_items(max_ext_bnd_items))

      registered_items(1:max_ext_bnd_items) = ''

   end subroutine

   function quantity_pli_combination_is_registered(quantity, location_file) result(is_registered)
      implicit none
      logical :: is_registered
      character(len=*), intent(in) :: quantity
      character(len=*), intent(in) :: location_file
      integer :: i
      character(len=max_registered_item_id) :: item_id

      item_id = trim(quantity)//'-'//trim(location_file)

      is_registered = .false.

      do i = 1, num_registered_items
         if (item_id == registered_items(i)) then
            is_registered = .true.
            return
         end if
      end do

   end function quantity_pli_combination_is_registered

   subroutine init_threttimes()

      use m_flow
      use m_flowgeom
      use fm_external_forcings_data
      use m_transport
      use m_sediment, only: stm_included
      use m_missing
      use m_find_name, only: find_name

      implicit none

      integer :: thrtlen, i, j, nseg, itrac, ifrac, iconst, n, ierr
      character(len=256) :: qidfm, tracnam, sedfracnam, qidnam

      ! deallocation of TH arrays
      if (allocated(threttim)) then
         deallocate (threttim)
      end if

      if (nopenbndsect == 0) then
         return
      end if

      allocate (threttim(NUMCONST, nopenbndsect), stat=ierr)
      call aerr('threttim(NUMCONST,nopenbndsect)', ierr, nopenbndsect)
      threttim = 0

      ! assign return times using temp arrays
      thrtlen = size(thrtt)
      do i = 1, thrtlen
         qidfm = thrtq(i)
         if (qidfm == 'salinitybnd' .and. allocated(kbnds)) then
            nseg = kbnds(5, thrtn(i))
            if (nseg /= i) cycle
            if (nseg == 0 .or. nseg > nopenbndsect) then
               write (msgbuf, '(i8,a)') thrtn(i), ' salinity boundary point is assigned to incorrect boundary segment'; call err_flush()
               cycle
            end if
            threttim(ISALT, nseg) = thrtt(i)
         else if (qidfm == 'temperaturebnd' .and. allocated(kbndtm)) then
            nseg = kbndtm(5, thrtn(i))
            if (nseg /= i) cycle
            if (nseg == 0 .or. nseg > nopenbndsect) then
               write (msgbuf, '(i8,a)') thrtn(i), ' temperature boundary point is assigned to incorrect boundary segment'; call err_flush()
               cycle
            end if
            threttim(ITEMP, nseg) = thrtt(i)
         else if (qidfm == 'sedimentbnd' .and. allocated(kbndsd) .and. .not. stm_included) then
            nseg = kbndsd(5, thrtn(i))
            if (nseg /= i) cycle
            if (nseg == 0 .or. nseg > nopenbndsect) then
               write (msgbuf, '(i8,a)') thrtn(i), ' sediment boundary point is assigned to incorrect boundary segment'; call err_flush()
               cycle
            end if
            do j = ISED1, ISEDN
               threttim(j, nseg) = thrtt(i)
            end do
         else if (qidfm(1:9) == 'tracerbnd') then
            call get_tracername(qidfm, tracnam, qidnam)
            itrac = find_name(trnames, tracnam)
            if (allocated(bndtr) .and. thrtn(i) <= nbndtr(itrac)) then
               nseg = bndtr(itrac)%k(5, thrtn(i))
               if (nseg /= i) cycle
               if (nseg == 0 .or. nseg > nopenbndsect) then
                  write (msgbuf, '(i8,a)') thrtn(i), ' tracer boundary point is assigned to incorrect boundary segment'; call err_flush()
                  cycle
               end if
               iconst = itrac2const(itrac)
               threttim(iconst, nseg) = thrtt(i)
            end if
         else if (qidfm(1:10) == 'sedfracbnd') then
            ierr = 0
            call get_sedfracname(qidfm, sedfracnam, qidnam)
            ifrac = find_name(sfnames, sedfracnam)
            if (allocated(bndsf) .and. thrtn(i) <= nbndsf(ifrac)) then ! i      = no of TH boundaries (i.e. 1 per fraction bnd)
               ! thrtn  = no of boundaries per fraction
               ! nbndsf = total no of bnd links per fractions
               nseg = bndsf(ifrac)%k(5, thrtn(i)) ! 5, has open bnd section where TH bnd applies
               !if (nseg /=i) cycle
               if (nseg == 0 .or. nseg > nopenbndsect) then
                  ierr = 1
               end if
               iconst = ifrac2const(ifrac)
               if (iconst == 0) cycle
               threttim(iconst, nseg) = thrtt(i)
            else
               ierr = 1
            end if
            if (ierr /= 0) then
               write (msgbuf, '(i8,a)') thrtn(i), ' sedfrac boundary point is assigned to incorrect boundary segment'; call err_flush()
               cycle
            end if
         end if
      end do

      if (allocated(thtbnds)) deallocate (thtbnds)
      if (allocated(thzbnds)) deallocate (thzbnds)
      if (allocated(thtbndtm)) deallocate (thtbndtm)
      if (allocated(thzbndtm)) deallocate (thzbndtm)
      if (allocated(thtbndsd)) deallocate (thtbndsd)
      if (allocated(thzbndsd)) deallocate (thzbndsd)

      allocate (thtbnds(nbnds), thzbnds(nbnds * kmxd), thtbndtm(nbndtm), thzbndtm(nbndtm * kmxd), thtbndsd(nbndsd), thzbndsd(nbndsd * kmxd), stat=ierr)
      call aerr('thtbnds(nbnds), thzbnds(nbnds*kmxd), thtbndtm(nbndtm), thzbndtm(nbndtm*kmxd), thtbndsd(nbndsd), thzbndsd(nbndsd*kmxd)', ierr, (kmxd + 1) * (nbnds + nbndtm + nbndsd))
      thzbnds = DMISS

      do i = 1, nbnds
         thtbnds(i) = threttim(ISALT, kbnds(5, i))
      end do

      do i = 1, nbndtm
         thtbndtm(i) = threttim(ITEMP, kbndtm(5, i))
      end do

      do i = 1, nbndsd
         thtbndsd(i) = threttim(ISED1, kbndsd(5, i))
      end do

      if (allocated(bndtr)) then
         do itrac = 1, numtracers
            iconst = itrac2const(itrac)

            if (allocated(bndtr(itrac)%tht)) deallocate (bndtr(itrac)%tht)
            if (allocated(bndtr(itrac)%thz)) deallocate (bndtr(itrac)%thz)

            n = nbndtr(itrac)

            allocate (bndtr(itrac)%tht(n), bndtr(itrac)%thz(n * kmxd), stat=ierr)
            call aerr('bndtr(itrac)%tht(n), bndtr(itrac)%thz(n*kmxd)', ierr, n * (kmxd + 1))

            bndtr(itrac)%thz = dmiss
            do i = 1, n
               bndtr(itrac)%tht(i) = threttim(iconst, bndtr(itrac)%k(5, i))
            end do
         end do
      end if

      if (allocated(bndsf)) then
         do ifrac = 1, numfracs
            if (allocated(bndsf(ifrac)%tht)) deallocate (bndsf(ifrac)%tht)
            if (allocated(bndsf(ifrac)%thz)) deallocate (bndsf(ifrac)%thz)

            n = nbndsf(ifrac)

            allocate (bndsf(ifrac)%tht(n), bndsf(ifrac)%thz(n * kmxd), stat=ierr)
            call aerr('bndsf(ifrac)%tht(n), bndsf(ifrac)%thz(n*kmxd)', ierr, n * (kmxd + 1))

            bndsf(ifrac)%thz = dmiss
            ! mapping to constituents, just in case fracs do not map sequentially to ised1 and so on
            iconst = ifrac2const(ifrac)
            if (iconst == 0) then
               bndsf(ifrac)%tht = 0d0
            else
               do i = 1, n
                  bndsf(ifrac)%tht(i) = threttim(iconst, bndsf(ifrac)%k(5, i))
               end do
            end if
         end do
      end if

   end subroutine init_threttimes

   subroutine allocatewindarrays()
      use m_wind
      use m_flow
      use m_flowgeom

      implicit none

      integer :: ierr

      if (.not. allocated(wx)) then
         allocate (wx(lnx), wy(lnx), wdsu(lnx), wdsu_x(lnx), wdsu_y(lnx), stat=ierr)
         call aerr('wx(lnx), wy(lnx), wdsu(lnx), wdsu_x(lnx), wdsu_y(lnx)', ierr, lnx)
         wx = 0d0
         wy = 0d0
         wdsu = 0d0
         wdsu_x = 0d0
         wdsu_y = 0d0
      end if

   end subroutine allocatewindarrays

!> Initializes boundaries and meteo for the current model.
!! @return Integer result status (0 if successful)
   function flow_initexternalforcings() result(iresult) ! This is the general hook-up to wind and boundary conditions
      use unstruc_model, only: md_extfile_new
      use dfm_error, only: DFM_NOERR
      integer :: iresult

      call setup(iresult)
      if (iresult == DFM_NOERR) then
         call init_new(md_extfile_new, iresult)
      end if
      if (iresult == DFM_NOERR) then
         call init_old(iresult)
      end if
      if (iresult == DFM_NOERR) then
         call finalize()
      end if

   end function flow_initexternalforcings

!> prepare all arrays that are necessary for both old and new external forcing. Only called as part of flow_initexternalforcings
   subroutine setup(iresult)
      use dfm_error, only: DFM_NOERR
      use m_transport, only: const_names
      use m_fm_wq_processes, only: wqbotnames
      use m_mass_balance_areas, only: mbaname
      use m_flowparameters, only: itempforcingtyp, btempforcingtypa, btempforcingtypc, btempforcingtyph, btempforcingtyps, btempforcingtypl, ja_friction_coefficient_time_dependent
      use m_flowtimes, only: refdat, julrefdat, timjan, handle_extra
      use m_flowgeom, only: ndx, lnx, lnxi, lne2ln, ln, xyen, nd, teta, kcu, kcs, iadv, lncn, ntheta
      use m_netw, only: xe, ye, zk
      use unstruc_model, only: md_inifieldfile
      use m_meteo
      use m_sediment, only: jaceneqtr, grainlay, mxgr
      use m_mass_balance_areas, only: mbadef, mbadefdomain, mbaname
      use dfm_error, only: dfm_extforcerror, dfm_wronginput, dfm_noerr, dfm_strerror
      use m_sobekdfm, only: init_1d2d
      use timespace_data, only: settimespacerefdat
      use timers, only: timstop, timstrt
      use unstruc_inifields, only: initialize_initial_fields
      use m_qnerror
      use m_flow_init_structurecontrol, only: flow_init_structurecontrol
      use m_setzminmax, only: setzminmax

      integer, intent(out) :: iresult

      integer :: ierr
      logical :: exist
      integer :: k, L, LF, KB, KBI, N, K2, iad, numnos, isf, mx, itrac
      integer, parameter :: N4 = 6
      character(len=256) :: rec
      integer :: tmp_nbndu, tmp_nbndt, tmp_nbndn

      iresult = DFM_NOERR

      tair_available = .false.
      dewpoint_available = .false.

      if (.not. allocated(const_names)) then
         allocate (const_names(0))
      end if
      if (.not. allocated(trnames)) then
         allocate (trnames(0))
      end if
      if (.not. allocated(wqbotnames)) then
         allocate (wqbotnames(0))
      end if
      if (.not. allocated(mbaname)) then
         allocate (mbaname(0))
      end if

      ! (re-)initialize flags/counters related to temperature forcings
      itempforcingtyp = 0
      btempforcingtypA = .false.
      btempforcingtypC = .false.
      btempforcingtypH = .false.
      btempforcingtypS = .false.
      btempforcingtypL = .false.

      ja_friction_coefficient_time_dependent = 0

      if (.not. allocated(sah)) then
         allocate (sah(ndx), stat=ierr)
         call aerr('sah(ndx)', ierr, ndx)
      end if

      call settimespacerefdat(refdat, julrefdat, Tzone, Timjan)

      inivel = 0 ! no initial velocity field loaded
      inivelx = 0
      inively = 0

      ! First initialize new-style StructureFile quantities.
      if (.not. flow_init_structurecontrol()) then
         iresult = DFM_EXTFORCERROR
         return
      end if

      ! First initialize new-style IniFieldFile quantities.
      if (len_trim(md_inifieldfile) > 0) then
         call timstrt('Init iniFieldFile', handle_extra(49)) ! initialize_initial_fields
         inquire (file=trim(md_inifieldfile), exist=exist)
         if (exist) then
            iresult = initialize_initial_fields(md_inifieldfile)
            if (iresult /= DFM_NOERR) then
               call timstop(handle_extra(49)) ! initialize_initial_fields
               return
            end if
         else
            call qnerror('Initial fields and parameters file '''//trim(md_inifieldfile)//''' not found.', '  ', ' ')
            write (msgbuf, '(a,a,a)') 'Initial fields and parameters file ''', trim(md_inifieldfile), ''' not found.'
            call warn_flush()
            iresult = DFM_EXTFORCERROR
            call timstop(handle_extra(49)) ! initialize_initial_fields
            return
         end if
         call timstop(handle_extra(49)) ! initialize_initial_fields
      end if

      if (jatimespace == 0) return ! Just cleanup and close ext file.

      if (allocated(ec_pwxwy_x)) deallocate (ec_pwxwy_x)
      if (allocated(ec_pwxwy_y)) deallocate (ec_pwxwy_y)
      if (allocated(patm)) deallocate (patm)
      if (allocated(kbndz)) deallocate (xbndz, ybndz, xy2bndz, zbndz, kbndz, zbndz0)
      if (allocated(zkbndz)) deallocate (zkbndz)
      id_first_wind = huge(id_first_wind)
      id_last_wind = -huge(id_last_wind)

      call realloc(lnxbnd, lnx - lnxi, keepExisting=.false., fill=0)

      if (nbndz > 0) then ! now you know the elementsets for the waterlevel bnds
         allocate (xbndz(nbndz), ybndz(nbndz), xy2bndz(2, nbndz), zbndz(nbndz), kbndz(N4, nbndz), zbndz0(nbndz), kdz(nbndz), stat=ierr)
         call aerr('xbndz(nbndz), ybndz(nbndz), xy2bndz(2,nbndz), zbndz(nbndz), kbndz(N4,nbndz), zbndz0(nbndz), kdz(nbndz)', ierr, nbndz * 10)
         if (jased > 0 .and. jaceneqtr == 2 .and. .not. stm_included) then
            if (allocated(zkbndz)) deallocate (zkbndz, kbanz)
            allocate (zkbndz(2, nbndz), stat=ierr)
            call aerr('zkbndz(2,nbndz)', ierr, 2 * nbndz)
            allocate (kbanz(2, nbndz), stat=ierr)
            call aerr('kban2(2,nbndz)', ierr, 2 * nbndz)
            kbanz = 0
         end if

         kbndz = 0; kdz = 1

         do k = 1, nbndz
            L = kez(k)
            Lf = lne2ln(L)
            kb = ln(1, Lf)
            kbi = ln(2, LF)

            xbndz(k) = xe(L) ! xz(kb)
            ybndz(k) = ye(L) ! yz(kb)
            zbndz0(k) = dmiss
            zbndz(k) = dmiss
            xy2bndz(:, k) = xyen(:, L)

            kbndz(1, k) = kb
            kbndz(2, k) = kbi
            kbndz(3, k) = Lf
            kbndz(4, k) = itpez(k)
            kbndz(5, k) = itpenz(k)
            kbndz(6, k) = ftpet(k)

         !! hier vullen

            lnxbnd(Lf - lnxi) = itpenz(k)

            do n = 1, nd(kbi)%lnx
               L = abs(nd(kbi)%ln(n))
               teta(L) = 1d0
            end do

            if (iadvec /= 0 .and. kcu(L) == -1) then
               iad = iadvec1D
            else
               iad = iadvec
            end if
            if (iad /= 0) then
               iadv(Lf) = 6 ! piaczeck upw
            else
               iadv(Lf) = 0
            end if

            if (jased > 0 .and. jaceneqtr == 2 .and. .not. stm_included) then
               zkbndz(1, k) = zk(lncn(1, Lf))
               zkbndz(2, k) = zk(lncn(2, Lf))
            end if
         end do

         do k = 1, nbndz
            kbi = kbndz(2, k)
            Lf = kbndz(3, k)
            if (iadvec /= 0 .and. kcu(Lf) == -1) then
               iad = iadvec1D
            else
               iad = iadvec
            end if

            do k2 = 1, nd(kbi)%lnx
               L = abs(nd(kbi)%ln(k2))
               if (L /= Lf) then
                  if (iad /= 0) then
                     iadv(L) = 6 ! piaczeck upw
                  else
                     iadv(L) = 0
                  end if
               end if
            end do
         end do
      end if

      if (allocated(kbndu)) deallocate (xbndu, ybndu, xy2bndu, zbndu, kbndu, zbndu0)
      if (allocated(zkbndu)) deallocate (zkbndu)
      if (allocated(zbndq)) deallocate (zbndq)
      if (allocated(sigmabndu)) deallocate (sigmabndu)
      if (allocated(zminmaxu)) deallocate (zminmaxu)

      ! allocate the following even if not needed (for debugging purposes)
      tmp_nbndu = max(nbndu, 1)
      allocate (xbndu(tmp_nbndu), ybndu(tmp_nbndu), xy2bndu(2, tmp_nbndu), kbndu(N4, tmp_nbndu), kdu(tmp_nbndu), stat=ierr)
      call aerr('xbndu(tmp_nbndu), ybndu(tmp_nbndu), xy2bndu(2,tmp_nbndu), kbndu(N4,tmp_nbndu), kdu(tmp_nbndu)', ierr, tmp_nbndu * (N4 + 5))
      if (jased > 0 .and. jaceneqtr == 2 .and. .not. stm_included) then
         if (allocated(zkbndu)) deallocate (zkbndu, kbanu)
         allocate (zkbndu(2, tmp_nbndu), stat=ierr)
         call aerr('zkbndu(2,tmp_nbndu)', ierr, 2 * tmp_nbndu)
         allocate (kbanu(2, tmp_nbndu), stat=ierr)
         call aerr('kbanu (2,tmp_nbndu)', ierr, 2 * tmp_nbndu)
         kbanu = 0
      end if

      allocate (zbndu(tmp_nbndu * kmxd), stat=ierr)
      call aerr('zbndu    (tmp_nbndu*kmxd)', ierr, tmp_nbndu * kmxd)
      allocate (zbndu0(tmp_nbndu * kmxd), stat=ierr) ! TODO: Spee/Reyns: the zbndu array was made 3D by Spee, but Reyns's zbndu0 changes have not been updated for this yet.
      call aerr('zbndu0   (tmp_nbndu*kmxd)', ierr, tmp_nbndu * kmxd)

      allocate (zbndq(tmp_nbndu), stat=ierr)
      call aerr('zbndq    (tmp_nbndu)', ierr, tmp_nbndu)

      allocate (zminmaxu(tmp_nbndu * 2), stat=ierr)
      call aerr('zminmaxu (tmp_nbndu*2  )', ierr, tmp_nbndu * 2)
      if (kmx > 0) then ! only used in 3D:
         allocate (sigmabndu(tmp_nbndu * kmxd), stat=ierr)
         call aerr('sigmabndu(tmp_nbndu*kmxd)', ierr, tmp_nbndu * kmxd)
      end if

      if (nbndu > 0) then ! similar for u bnd's
         kbndu = 0; kdu = 1
         do k = 1, nbndu
            L = keu(k)
            Lf = lne2ln(L)
            kb = ln(1, Lf)
            kbi = ln(2, Lf)
            xbndu(k) = xe(L) ! xz(kb)
            ybndu(k) = ye(L) ! yz(kb)
            xy2bndu(:, k) = xyen(:, L)

            kbndu(1, k) = kb
            kbndu(2, k) = kbi
            kbndu(3, k) = Lf
            kbndu(4, k) = itpeu(k)
            kbndu(5, k) = itpenu(k)
            kbndu(6, k) = ftpet(k) ! riemann relaxation time

            lnxbnd(Lf - lnxi) = itpenu(k)

            do n = 1, nd(kbi)%lnx
               L = abs(nd(kbi)%ln(n))
               teta(L) = 1d0
            end do

            iadv(Lf) = -1 ! switch off adv at open u-bnd's

            if (jased > 0 .and. jaceneqtr == 2 .and. .not. stm_included) then
               zkbndu(1, k) = zk(lncn(1, Lf))
               zkbndu(2, k) = zk(lncn(2, Lf))
            end if

         end do
      end if

      if (allocated(kbnds)) deallocate (xbnds, ybnds, xy2bnds, zbnds, kbnds)
      if (jasal > 0) then
         if (allocated(sigmabnds)) deallocate (sigmabnds)
         if (allocated(zminmaxs)) deallocate (zminmaxs)
         if (nbnds > 0) then ! salinity as for waterlevel bnds, but no kcs = -1
            numnos = 0
            allocate (xbnds(nbnds), ybnds(nbnds), xy2bnds(2, nbnds), zbnds(kmxd * nbnds), kbnds(5, nbnds), kds(nbnds), stat=ierr)
            call aerr('xbnds(nbnds), ybnds(nbnds), xy2bnds(2,nbnds), zbnds(kmxd*nbnds), kbnds(5,nbnds), kds(nbnds)', ierr, nbnds * 9)
            ! also allocate 3D-sigma bnd distribution for EC
            allocate (sigmabnds(kmxd * nbnds))
            call aerr('sigmabnds(kmxd*nbnds)', ierr, kmxd * nbnds)
            allocate (zminmaxs(2 * nbnds))
            call aerr('zminmaxs (2*nbnds)', ierr, 2 * nbnds)

            zbnds = DMISS; kbnds = 0; kds = 1
            do k = 1, nbnds
               L = kes(k)
               Lf = lne2ln(L)
               if (Lf <= 0 .or. Lf > lnx) then
                  numnos = numnos + 1
                  cycle
               end if
               kb = ln(1, Lf)
               kbi = ln(2, Lf)
               if (kcs(kb) < 0) then ! if already opened by flow bnd's
                  xbnds(k) = xe(L) ! xz(kb)
                  ybnds(k) = ye(L) ! yz(kb)
                  xy2bnds(:, k) = xyen(:, L)

                  kbnds(1, k) = kb
                  kbnds(2, k) = kbi
                  kbnds(3, k) = Lf
                  kbnds(5, k) = lnxbnd(Lf - lnxi)
               end if
            end do
            if (numnos > 0) then
               rec = ' '
               write (rec, '(a,i6,a)') '(', numnos, ' points)'
               call qnerror('Salinity boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
               iresult = DFM_WRONGINPUT
               return
            end if
         end if
      end if

      if (allocated(kbndTM)) deallocate (xbndTM, ybndTM, xy2bndTM, zbndTM, kbndTM)
      if (jatem > 0) then
         if (allocated(sigmabndTM)) deallocate (sigmabndTM)
         if (allocated(zminmaxTM)) deallocate (zminmaxTM)
         if (nbndTM > 0) then ! salinity as for waterlevel bnds, but no kcs = -1
            numnos = 0
            allocate (xbndTM(nbndTM), ybndTM(nbndTM), xy2bndTM(2, nbndTM), zbndTM(kmxd * nbndTM), kbndTM(5, nbndTM), kdTM(nbndTM), stat=ierr)
            call aerr('xbndTM(nbndTM), ybndTM(nbndTM), xy2bndTM(2,nbndTM), zbndTM(kmxd*nbndTM), kbndTM(5,nbndTM), kdTM(nbndTM)', ierr, nbndTM * 9)
            ! also allocate 3D-sigma bnd distribution for EC
            allocate (sigmabndTM(kmxd * nbndTM), stat=ierr)
            call aerr('sigmabndTM(kmxd*nbndTM)', ierr, kmxd * nbndTM)
            allocate (zminmaxTM(2 * nbndTM), stat=ierr)
            call aerr('zminmaxTM(2*nbndTM)', ierr, 2 * nbndTM)
            zbndTM = DMISS; kbndTM = 0; kdTM = 1
            do k = 1, nbndTM
               L = keTM(k)
               Lf = lne2ln(L)
               if (Lf <= 0 .or. Lf > lnx) then
                  numnos = numnos + 1
                  cycle
               end if
               kb = ln(1, Lf)
               kbi = ln(2, Lf)
               if (kcs(kb) < 0) then ! if already opened by flow bnd's
                  xbndTM(k) = xe(L) ! xz(kb)
                  ybndTM(k) = ye(L) ! yz(kb)
                  xy2bndTM(:, k) = xyen(:, L)
                  kbndTM(1, k) = kb
                  kbndTM(2, k) = kbi
                  kbndTM(3, k) = Lf
                  kbndTM(5, k) = lnxbnd(Lf - lnxi)
               else
                  call qnerror("flow_initexternalforcings/TM: boundary not opened yet", ' ', ' ')
                  continue
               end if
            end do
            if (numnos > 0) then
               rec = ' '
               write (rec, '(a,i6,a)') '(', numnos, ' points)'
               call qnerror('Temperature boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
               iresult = DFM_WRONGINPUT
               return
            end if
         end if
      end if

      call init_1d2d()

      ! JRE ================================================================
      if (nbndw > 0 .and. .not. (jawave == 4)) then
         call qnerror('Wave energy boundary defined without setting correct wavemodelnr.', ' ', ' ')
         iresult = DFM_WRONGINPUT
      end if
      if (nbndw > 0) then
         numnos = 0
         call mess(LEVEL_INFO, 'Enabled wave forcing while reading external forcings.')
         if (allocated(kbndw)) deallocate (xbndw, ybndw, xy2bndw, zbndw, kbndw)
         allocate (xbndw(nbndw), ybndw(nbndw), xy2bndw(2, nbndw), zbndw(ntheta, nbndw), kbndw(4, nbndw), kdw(nbndw), stat=ierr)
         call aerr('xbndw(nbndw), ybndw(nbndw), xy2bndw(2,nbndw), zbndw(ntheta,nbndw), kbndw(4,nbndw), kdw(nbndw)', ierr, nbndw * (9 + ntheta))
         kbndw = 0; kdw = 1
         do k = 1, nbndw
            L = kew(k)
            Lf = lne2ln(L)
            if (Lf <= 0 .or. Lf > lnx) then
               numnos = numnos + 1
               cycle
            end if
            kb = ln(1, Lf)
            kbi = ln(2, Lf)
            if (kcs(kb) < 0) then ! if already opened by flow bnd's
               xbndw(k) = xe(L) !xz(kb)
               ybndw(k) = ye(L) !yz(kb)
               xy2bndw(:, k) = xyen(:, L)
               kbndw(1, k) = kb
               kbndw(2, k) = kbi
               kbndw(3, k) = Lf
            end if
         end do
         if (numnos > 0) then
            rec = ' '
            write (rec, '(a,i6,a)') '(', numnos, ' points)'
            call qnerror('Wave energy boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
            iresult = DFM_WRONGINPUT
            return
         end if

      end if
! ========================

      if (allocated(kbndsd)) deallocate (xbndsd, ybndsd, xy2bndsd, zbndsd, kbndsd)
      if (allocated(sigmabndsd)) deallocate (sigmabndsd)
      if (allocated(zminmaxsd)) deallocate (zminmaxsd)
      if (nbndsd > 0) then ! sediment bnds as for waterlevel bnds, but no kcs = -1
         numnos = 0
         allocate (xbndsd(nbndsd), ybndsd(nbndsd), xy2bndsd(2, nbndsd), zbndsd(nbndsd), kbndsd(5, nbndsd), kdsd(nbndsd), stat=ierr)
         call aerr('xbndsd(nbndsd), ybndsd(nbndsd), xy2bndsd(2,nbndsd), zbndsd(nbndsd), kbndsd(5,nbndsd), kdsd(nbndsd)', ierr, nbndsd * 9)
         ! also allocate 3D-sigma bnd distribution for EC
         allocate (sigmabndsd(kmxd * nbndsd), stat=ierr)
         call aerr('sigmabndsd(kmxd*nbndsd)', ierr, kmxd * nbndsd)
         allocate (zminmaxsd(2 * nbndsd), stat=ierr)
         call aerr('zminmaxsd(2*nbndsd)', ierr, 2 * nbndsd)

         kbndsd = 0; kdsd = 1

         do k = 1, nbndsd
            L = kesd(k)
            Lf = lne2ln(L)
            if (Lf <= 0 .or. Lf > lnx) then
               numnos = numnos + 1
               cycle
            end if
            kb = ln(1, Lf)
            kbi = ln(2, Lf)
            if (kcs(kb) < 0) then ! if already opened by flow bnd's
               xbndsd(k) = xe(L) ! xz(kb)
               ybndsd(k) = ye(L) ! yz(kb)
               xy2bndsd(:, k) = xyen(:, L)
               kbndsd(1, k) = kb
               kbndsd(2, k) = kbi
               kbndsd(3, k) = Lf
               kbndsd(5, k) = lnxbnd(Lf - lnxi)
            end if
         end do
         if (numnos > 0) then
            rec = ' '
            write (rec, '(a,i6,a)') '(', numnos, ' points)'
            call qnerror('Sediment boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
            iresult = DFM_WRONGINPUT
            return
         end if
      end if

      ! tracers
      if (nbndtr_all > 0) then ! sediment bnds as for waterlevel bnds, but no kcs = -1
         call dealloc_bndarr(bndtr)
         allocate (bndtr(numtracers))

         do itrac = 1, numtracers
            numnos = 0
            call alloc_bnd(nbndtr(itrac), kmx, bndtr(itrac))

            do k = 1, nbndtr(itrac)
               L = ketr(k, itrac)
               Lf = lne2ln(L)
               if (Lf <= 0 .or. Lf > lnx) then
                  numnos = numnos + 1
                  cycle
               end if
               kb = ln(1, Lf)
               kbi = ln(2, Lf)
               if (kcs(kb) < 0) then ! if already opened by flow bnd's
                  bndtr(itrac)%name = trim(trnames(itrac))
                  bndtr(itrac)%x(k) = xe(L) ! xz(kb)
                  bndtr(itrac)%y(k) = ye(L) ! yz(kb)
                  bndtr(itrac)%xy2(:, k) = xyen(:, L)
                  bndtr(itrac)%k(1, k) = kb
                  bndtr(itrac)%k(2, k) = kbi
                  bndtr(itrac)%k(3, k) = Lf
                  bndtr(itrac)%k(5, k) = lnxbnd(Lf - lnxi)
               end if

               if (numnos > 0) then
                  rec = ' '
                  write (rec, '(a,i6,a)') '(', numnos, ' points)'
                  call qnerror('Tracer boundary for '''//trim(bndtr(itrac)%name)//''' (partially) unassociated. ', trim(rec), ' Open boundary required.')
                  iresult = DFM_WRONGINPUT
                  return
               end if
            end do
            ! also allocate 3D-sigma bnd distribution for EC
            call realloc(bndtr(itrac)%sigma, kmxd * nbndtr(itrac), stat=ierr, fill=0d0)
            call aerr('sigma(kmxd*nbndtr(itrac))', ierr, kmxd * nbndtr(itrac))
            call realloc(bndtr(itrac)%zminmax, 2 * nbndtr(itrac), stat=ierr, fill=0d0)
            call aerr('bndtr(itrac)%zminmax(2*nbndtr(itrac))', ierr, 2 * nbndtr(itrac))
         end do ! itrac
      end if

      if (stm_included) then
         if (nbndsf_all > 0) then
            call dealloc_bndarr(bndsf)
            allocate (bndsf(numfracs))

            do isf = 1, numfracs
               numnos = 0
               call alloc_bnd(nbndsf(isf), kmx, bndsf(isf)) ! 2D only for now
               do k = 1, nbndsf(isf)
                  L = kesf(k, isf)
                  Lf = lne2ln(L)
                  if (Lf <= 0 .or. Lf > lnx) then
                     numnos = numnos + 1
                     cycle
                  end if
                  kb = ln(1, Lf)
                  kbi = ln(2, Lf)
                  if (kcs(kb) < 0) then ! if already opened by flow bnd's
                     bndsf(isf)%name = trim(sfnames(isf))
                     bndsf(isf)%x(k) = xe(L) ! xz(kb)
                     bndsf(isf)%y(k) = ye(L) ! yz(kb)
                     bndsf(isf)%xy2(:, k) = xyen(:, L)
                     bndsf(isf)%k(1, k) = kb
                     bndsf(isf)%k(2, k) = kbi
                     bndsf(isf)%k(3, k) = Lf
                     bndsf(isf)%k(5, k) = lnxbnd(Lf - lnxi)
                  end if

                  if (numnos > 0) then
                     rec = ' '
                     write (rec, '(a,i6,a)') '(', numnos, ' points)'
                     call qnerror('Sediment fraction boundary for '''//trim(bndsf(isf)%name)//''' (partially) unassociated. ', trim(rec), ' Open boundary required.')
                     iresult = DFM_WRONGINPUT
                     return
                  end if
               end do ! nbndsf(isf)
               ! also allocate 3D-sigma bnd distribution for EC
               call realloc(bndsf(isf)%sigma, kmxd * nbndsf(isf), stat=ierr, fill=0d0)
               call aerr('sigma(kmxd*nbndsf(isf))', ierr, kmxd * nbndsf(isf))
               call realloc(bndsf(isf)%zminmax, 2 * nbndsf(isf), stat=ierr, fill=0d0)
               call aerr('bndsf(isf)%zminmax(2*nbndsf(isf))', ierr, 2 * nbndsf(isf))
            end do ! ised
         end if
      end if

      if (allocated(kbndt)) deallocate (xbndt, ybndt, xy2bndt, zbndt, kbndt)
      ! allocate the following even if not needed (for debugging purposes)
      tmp_nbndt = max(nbndt, 1)
      allocate (xbndt(tmp_nbndt), ybndt(tmp_nbndt), xy2bndt(2, tmp_nbndt), zbndt(tmp_nbndt), kbndt(4, tmp_nbndt), kdt(tmp_nbndt), stat=ierr)
      call aerr('xbndt(tmp_nbndt), ybndt(tmp_nbndt), xy2bndt(2,tmp_nbndt), zbndt(tmp_nbndt), kbndt(4,tmp_nbndt), kdt(tmp_nbndt)', ierr, tmp_nbndt * 10)

      if (nbndt > 0) then ! Tangential velocity boundaries as u bnds
         numnos = 0
         kbndt = 0; kdt = 1
         do k = 1, nbndt
            L = ket(k)
            Lf = lne2ln(L)
            if (Lf <= 0 .or. Lf > lnx) then
               numnos = numnos + 1
               cycle
            end if
            kb = ln(1, Lf)
            kbi = ln(2, Lf)
            if (kcs(kb) < 0) then ! if already opened by flow bnd's
               xbndt(k) = xe(L) ! xz(kb)
               ybndt(k) = ye(L) ! yz(kb)
               xy2bndt(:, k) = xyen(:, L)
               kbndt(1, k) = kb
               kbndt(2, k) = kbi
               kbndt(3, k) = Lf
            end if
         end do
         if (numnos > 0) then
            rec = ' '
            write (rec, '(a,i6,a)') '(', numnos, ' points)'
            call qnerror('Tangential boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
            iresult = DFM_WRONGINPUT
            return
         end if
      end if

      if (allocated(kbnduxy)) deallocate (xbnduxy, ybnduxy, xy2bnduxy, zbnduxy, kbnduxy)
      if (allocated(sigmabnduxy)) deallocate (sigmabnduxy)
      if (allocated(zminmaxuxy)) deallocate (zminmaxuxy)
      if (nbnduxy > 0) then ! Tangential velocity boundaries as u bnds
         numnos = 0
         allocate (xbnduxy(nbnduxy), ybnduxy(nbnduxy), xy2bnduxy(2, nbnduxy), zbnduxy(2 * kmxd * nbnduxy), kbnduxy(4, nbnduxy), kduxy(nbnduxy), stat=ierr)
         call aerr('xbnduxy(nbnduxy), ybnduxy(nbnduxy), xy2bnduxy(2,nbnduxy), zbnduxy(2*kmxd*nbnduxy), kbnduxy(4,nbnduxy), kduxy(nbnduxy)', ierr, nbnduxy * 10)
         ! also allocate 3D-sigma bnd distribution for EC
         allocate (sigmabnduxy(kmxd * nbnduxy), stat=ierr)
         call aerr('sigmabnduxy(kmxd*nbnduxy)', ierr, kmxd * nbnduxy)
         allocate (zminmaxuxy(2 * nbnduxy), stat=ierr)
         call aerr('zminmaxuxy(2*nbnduxy)', ierr, 2 * nbnduxy)

         zbnduxy(:) = 0
         kbnduxy = 0
         kduxy = 1
         do k = 1, nbnduxy
            L = keuxy(k)
            Lf = lne2ln(L)
            if (Lf <= 0 .or. Lf > lnx) then
               numnos = numnos + 1
               cycle
            end if
            kb = ln(1, Lf)
            kbi = ln(2, Lf)
            if (kcs(kb) < 0) then ! if already opened by flow bnd's
               xbnduxy(k) = xe(L)
               ybnduxy(k) = ye(L)
               xy2bnduxy(:, k) = xyen(:, L)
               kbnduxy(1, k) = kb
               kbnduxy(2, k) = kbi
               kbnduxy(3, k) = Lf
            end if
         end do
         if (numnos > 0) then
            rec = ' '
            write (rec, '(a,i6,a)') '(', numnos, ' points)'
            call qnerror('UxUy velocity boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
            iresult = DFM_WRONGINPUT
            return
         end if
      end if

      if (allocated(kbndn)) deallocate (xbndn, ybndn, xy2bndn, zbndn, kbndn)
      ! allocate the following even if not needed (for debugging purposes)
      tmp_nbndn = max(nbndn, 1)
      allocate (xbndn(tmp_nbndn), ybndn(tmp_nbndn), xy2bndn(2, tmp_nbndn), zbndn(tmp_nbndn), kbndn(4, tmp_nbndn), kdn(tmp_nbndn), stat=ierr)
      call aerr('xbndn(tmp_nbndn), ybndn(tmp_nbndn), xy2bndn(2,tmp_nbndn), zbndn(tmp_nbndn), kbndn(4,tmp_nbndn), kdn(tmp_nbndn)', ierr, tmp_nbndn * 10)
      if (nbndn > 0) then ! Normal velocity boundaries as z bnds
         numnos = 0
         kbndn = 0; kdn = 1
         do k = 1, nbndn
            L = ken(k)
            Lf = lne2ln(L)
            if (Lf <= 0 .or. Lf > lnx) then
               numnos = numnos + 1
               cycle
            end if
            kb = ln(1, Lf)
            kbi = ln(2, Lf)
            if (kcs(kb) < 0) then ! if already opened by flow bnd's
               xbndn(k) = xe(L) ! xz(kb)
               ybndn(k) = ye(L) ! yz(kb)
               xy2bndn(:, k) = xyen(:, L)
               kbndn(1, k) = kb
               kbndn(2, k) = kbi
               kbndn(3, k) = Lf
               iadv(Lf) = 77
            end if
         end do
         if (numnos > 0) then
            rec = ' '
            write (rec, '(a,i6,a)') '(', numnos, ' points)'
            call qnerror('Normal boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
            iresult = DFM_WRONGINPUT
            return
         end if
      end if

      if (allocated(xe)) then
         deallocate (xyen, xe, ye)
      end if

      if (nshiptxy > 0) then
         if (allocated(shx)) deallocate (xyship, shx, shy, shu, shv, shi, sho)
         allocate (xyship(2 * nshiptxy), shx(nshiptxy), shy(nshiptxy), shu(nshiptxy), shv(nshiptxy), shi(nshiptxy), sho(nshiptxy), stat=ierr)
         call aerr('xyship(2*nshiptxy), shx(nshiptxy), shy(nshiptxy), shu(nshiptxy), shv(nshiptxy), shi(nshiptxy), sho(nshiptxy)', ierr, 4 * nshiptxy)
         iniship = 0; nshiptxy = 0; shx = 0d0; shy = 0d0; xyship = dmiss
      end if

      if (jased > 0) then
         mx = size(grainlay, 2)
         allocate (grainlayerthickness(mx, mxgr), stat=ierr)
         call aerr('grainlayerthickness(mx,mxgr)', ierr, mx * mxgr)
         grainlayerthickness = dmiss
      else
         mxgr = 0 ! jre dangerous...
      end if

      call setzminmax(); call setsigmabnds() ! our side of preparation for 3D ec module

      ! initialise mass balance areas - always allocate these arrays
      call realloc(mbadef, Ndkx, keepExisting=.false., fill=-999)
      call realloc(mbadefdomain, Ndkx, keepExisting=.false., fill=-999)

      if (kmx > 0) then
         if (jastructurelayersactive > 0) then
            if (allocated(ff3)) deallocate (ff3)
            allocate (ff3(3, 0:kmxd)) ! and wait till similar lines appear in the %environment
         end if
      end if

      if (iresult /= DFM_NOERR) then
         call mess(LEVEL_WARN, 'Error during initialisation of External Forcings. See message:')
         call dfm_strerror(msgbuf, iresult)
         call warn_flush()
      end if

   end subroutine setup

   !> Clean up after initialization, deallocate temporary arrays and check for any deprecated or not accessed keywords. Only called as part of fm_initexternalforcings
   subroutine finalize()
      use m_flowgeom, only: ndx, lnx, csu, snu, jagrounlay, wigr, argr, pergr, lnx1d, grounlay, grounlayuni, prof1d, ndxi, lnxi, ln, ba, bare, ndx2d, kcu, dx, bl, kcs, xz, yz
      use m_flowtimes, only: ti_mba
      use m_storage, only: t_storage, get_surface
      use m_structures, only: network
      use m_meteo
      use m_sediment, only: mxgr, grainlay, uniformerodablethickness, jagrainlayerthicknessspecified
      use m_transport, only: numconst_mdu, numconst
      use m_mass_balance_areas, only: mbaname, nomba, mbadef, mbadefdomain
      use m_partitioninfo, only: jampi, idomain, my_rank, reduce_int_sum, set_japartqbnd
      use m_crosssections, only: cs_type_normal, getcsparstotal
      use m_trachy, only: trachy_resistance
      use m_structures, only: check_model_has_structures_across_partitions
      use m_laterals, only: initialize_lateraldata
      use m_get_kbot_ktop
      use m_get_prof_1D
      use mathconsts, only: pi
      use m_filez, only: doclose

      integer :: j, k, ierr, l, n, itp, kk, k1, k2, kb, kt, nstor, i, ja
      integer :: imba, needextramba, needextrambar
      logical :: hyst_dummy(2)
      real(kind=dp) :: area, width, hdx
      type(t_storage), pointer :: stors(:)

      ! Cleanup:
      if (jafrculin == 0 .and. allocated(frculin)) then
         deallocate (frculin)
      end if

      if (allocated(kez)) then ! mext > 0 .or. len_trim(md_extfile_new) > 0) then
         deallocate (kez, keu, kes, ketm, kesd, ket, keuxy, ken, ke1d2d, keg, ked, kep, kedb, keklep, kevalv, kegs, kegen, itpez, itpenz, itpeu, itpenu, kew, ketr)
      end if

      if (mext /= 0) then
         call doclose(mext) ! close ext file
      end if

      if (allocated(kdz)) deallocate (kdz)
      if (allocated(kdu)) deallocate (kdu)
      if (allocated(kds)) deallocate (kds)
      if (allocated(kdTM)) deallocate (kdTM)
      if (allocated(kdw)) deallocate (kdw)
      if (allocated(kdsd)) deallocate (kdsd)
      if (allocated(kdt)) deallocate (kdt)
      if (allocated(kduxy)) deallocate (kduxy)
      if (allocated(kdn)) deallocate (kdn)
      if (allocated(kdg)) deallocate (kdg)
      if (allocated(kdd)) deallocate (kdd)
      if (allocated(kdgen)) deallocate (kdgen)
      if (allocated(kdp)) deallocate (kdp)

      if (allocated(xy2gate)) deallocate (xy2gate)
      if (allocated(xy2cdam)) deallocate (xy2cdam)
      if (allocated(xy2cgen)) deallocate (xy2cgen)

      if (allocated(xy2pump)) deallocate (xy2pump)

      if (mxgr > 0 .and. .not. stm_included) then
         do j = 1, mxgr
            grainlay(j, :) = uniformerodablethickness(j)
         end do

         if (jagrainlayerthicknessspecified == 1) then
            do k = 1, size(grainlay, 2)
               do j = 1, mxgr
                  if (grainlayerthickness(k, j) /= dmiss) then
                     grainlay(j, k) = grainlayerthickness(k, j)
                  end if
               end do
            end do
            deallocate (grainlayerthickness)
         end if
      end if

      if (jawind == 0) then
         if (jawave > 0 .and. jawave < 3) then
            jawave = 0 ! no wind, no waves
            call mess(LEVEL_INFO, 'No wind, so waves is switched off ')
         end if
         if (jatem > 1) then
            jatem = 1 ! no wind, no heat model temperature
            call mess(LEVEL_INFO, 'No wind ?? => no heat model !')
         end if
      end if

      if (ja_computed_airdensity == 1) then
         if ((japatm /= 1) .or. .not. tair_available .or. .not. dewpoint_available .or. &
             (item_atmosphericpressure == ec_undef_int) .or. (item_airtemperature == ec_undef_int) .or. (item_humidity == ec_undef_int)) then
            call mess(LEVEL_ERROR, 'Quantities airpressure, airtemperature and dewpoint are expected, as separate quantities (e.g., QUANTITY = airpressure), in ext-file in combination with keyword computedAirdensity in mdu-file.')
         else
            if (ja_airdensity == 1) then
               call mess(LEVEL_ERROR, 'Quantity airdensity in ext-file is unexpected in combination with keyword computedAirdensity = 1 in mdu-file.')
            elseif (jaroro > 1) then
               call mess(LEVEL_ERROR, 'Keyword RhoairRhowater > 1 is not allowed in combination with keyword computedAirdensity = 1 in mdu-file.')
            else
               allocate (airdensity(ndx), stat=ierr)
               call aerr('airdensity(ndx)', ierr, ndx)
               airdensity = 0d0
            end if
         end if
      end if

      if (javiusp == 1) then
         do L = 1, lnx
            if (viusp(L) == dmiss) then
               viusp(L) = vicouv
            end if
         end do
      end if

      if (jadiusp == 1) then
         do L = 1, lnx
            if (diusp(L) == dmiss) then
               diusp(L) = dicouv
            end if
         end do
      end if

      if (jaSecchisp > 0) then
         do n = 1, ndx
            if (Secchisp(n) == dmiss) then
               Secchisp(n) = Secchidepth
            end if
         end do
      end if

      if (inivel == 1) then
         do L = 1, lnx
            if (uxini(L) == dmiss .and. uyini(L) == dmiss) then
               cycle
            end if
            u1(L) = uxini(L) * csu(L) + uyini(L) * snu(L)
         end do
      else if (inivelx == 1) then
         ! only x component imposed
         do L = 1, lnx
            if (uxini(L) == dmiss) then
               cycle
            end if
            u1(L) = uxini(L) * csu(L)
         end do
      else if (inively == 1) then
         ! only y component imposed
         do L = 1, lnx
            if (uyini(L) == dmiss) then
               cycle
            end if
            u1(L) = uyini(L) * snu(L)
         end do
      end if

      if (allocated(uxini)) deallocate (uxini)
      if (allocated(uyini)) deallocate (uyini)

      if (javeg > 0) then
         call realloc(rnveg, Ndkx, keepExisting=.false., fill=0d0, stat=ierr)
         call aerr(' rnveg (Ndkx)', ierr, Ndkx)
         call realloc(diaveg, Ndkx, keepExisting=.false., fill=0d0, stat=ierr)
         call aerr('diaveg (Ndkx)', ierr, Ndkx)

         if (jaCdvegsp > 0) then
            call realloc(Cdvegsp, Ndkx, keepExisting=.false., fill=0d0, stat=ierr)
            call aerr('Cdvegsp (Ndkx)', ierr, Ndkx)
         end if

         javeg = 1
         if (.not. allocated(stemheight)) then
            call realloc(stemheight, Ndkx, keepExisting=.false., fill=0d0, stat=ierr)
            call aerr(' stemheight (Ndkx)', ierr, Ndkx)
         end if

         if (allocated(stemdiam) .and. allocated(stemdens)) then
            do k = 1, ndx
               if (stemdens(k) > 0d0) then
                  if ((pi * (stemdiam(k) / 2)**2 * stemdens(k)) > 1.0_dp) then
                     call mess(LEVEL_ERROR, 'The area covered by a plant or pile (based on the quantity "stemdiameter") is larger than the typical area of it (calculated as the reciprocal of the quantity "stemdensity").')
                  end if
                  rnveg(k) = stemdens(k)
                  diaveg(k) = stemdiam(k)
               end if
               if (stemheight(k) == dmiss) stemheight(k) = 0d0
            end do
            deallocate (stemdiam, stemdens)
         end if

         if (kmx == 0) then
            if (jabaptist >= 3) then
               call realloc(alfaveg, Lnx, keepExisting=.false., fill=0d0, stat=ierr)
               call aerr(' alfaveg (Lnx)', ierr, Lnx)
               call realloc(cfuveg, Lnx, keepExisting=.false., fill=0d0, stat=ierr)
               call aerr(' cfuveg  (Lnx)', ierr, Lnx)
            end if
            if (jabaptist >= 2) then
               call realloc(alfav, Lnx, keepExisting=.false., fill=0d0, stat=ierr)
               call aerr(' alfav (Lnx)', ierr, Lnx)
            end if
         end if

         if (rhoveg /= dmiss) then
            call realloc(phiv, Ndx, keepExisting=.false., fill=0d0, stat=ierr)
            call realloc(phivt, Ndx, keepExisting=.false., fill=0d0, stat=ierr)
         end if
      end if

      if ((jatrt > 0) .and. trachy_resistance) then
         call realloc(alfav, Lnx, keepExisting=.false., fill=0d0, stat=ierr)
         call aerr(' alfav (Lnx)', ierr, Lnx)
      end if

      if (jagrounLay == 1) then
         if (allocated(wigr)) deallocate (wigr, argr, pergr)
         allocate (argr(lnx1D), stat=ierr); argr = 0d0
         call aerr('argr(lnx1D)', ierr, Lnx1D)
         allocate (wigr(lnx1D), stat=ierr); wigr = 0d0
         call aerr('wigr(lnx1D)', ierr, Lnx1D)
         allocate (pergr(lnx1D), stat=ierr); pergr = 0d0
         call aerr('pergr(lnx1D)', ierr, Lnx1D)
         do L = 1, lnx1D
            if (grounlay(L) == dmiss) then
               if (grounlayuni > 0) then
                  grounlay(L) = grounlayuni
               else
                  grounlay(L) = 0d0
               end if
            end if
         end do
         jagrounlay = 0
         do L = 1, lnx1D
            itp = prof1D(3, L)
            if (grounlay(L) > 0d0 .and. abs(itp) <= 3) then
               call getprof_1D(L, grounlay(L), argr(L), wigr(L), 1, 1, pergr(L))
            end if
         end do
         jagrounlay = 1
      else
         if (allocated(grounlay)) deallocate (grounlay)
      end if

      if (jampi == 1) then
         ! see if one or more discharge boundaries are partioned
         call set_japartqbnd()
         if (japartqbnd /= 0) call mess(LEVEL_WARN, 'One or more discharge boundaries are partitioned.')
      else
         japartqbnd = 0
      end if

      ! For parallel simulation initialize array ibnd_own and scalar ndxbnd_own, discarding ghost boundary points
      if (jampi > 0) then
         ndxbnd_own = 0 ! Nr. of boundary points without ghost boundary points
         call realloc(ibnd_own, ndx - ndxi, stat=ierr, keepExisting=.false.)
         ibnd_own = 0
         do n = 1, ndx - ndxi
            kk = ln(2, lnxi + n) ! kk is the interior cell that connects by link lnxi+n to the current boundary points
            if (idomain(kk) == my_rank) then
               ndxbnd_own = ndxbnd_own + 1
               ibnd_own(ndxbnd_own) = n
            end if
         end do
      end if

      if (allocated(frcuroofs)) then
         do L = 1, lnxi
            if (frcuroofs(L) /= dmiss) then
               frcu(L) = frcuroofs(L)
            end if
         end do
         deallocate (frcuroofs)
      end if

      if (allocated(infiltcaproofs)) then
         do n = 1, ndxi
            if (infiltcaproofs(n) /= dmiss) then
               infiltcap(n) = infiltcaproofs(n)
            end if
         end do
         deallocate (infiltcaproofs)
      end if

      if (jaevap == 0 .and. jarain == 0) then
         a1ini = sum(ba(1:ndxi))
      else
         if (allocated(bare)) deallocate (bare)
         allocate (bare(ndxi), stat=ierr) ! base area for rainfall / evaporation
         call aerr('bare(ndxi)', ierr, ndx); 
         bare(1:ndxi) = ba(1:ndxi)

         if (network%loaded) then
            bare(ndx2D + 1:ndxi) = 0d0
            do L = 1, lnx1D ! for all links, set link width
               k1 = ln(1, L)
               k2 = ln(2, L)
               if (kcu(L) == 1) then
                  ! Calculate maximal total area by using a water depth of 1000 m. FOR BARE we need the maximal possible catchment area.
                  ! For this reason the total width is used and also the area of the storage nodes is added tot BARE.
                  ! Since BA contains the flow area only and not the total area or the area of the storage nodes, BARE has to be recalculated.

                  hyst_dummy = .false.
                  call GetCSParsTotal(network%adm%line2cross(L, 2), network%crs%cross, 1d3, area, width, CS_TYPE_NORMAL, hyst_dummy)

                  hdx = 0.5d0 * dx(L)
                  if (k1 > ndx2d) bare(k1) = bare(k1) + hdx * width
                  if (k2 > ndx2d) bare(k2) = bare(k2) + hdx * width
               end if
            end do
         end if

         nstor = network%stors%count
         if (nstor > 0) then
            stors => network%stors%stor
            do i = 1, nstor
               k1 = stors(i)%grid_point
               if (k1 > 0) then
                  ! Add storage area to BARE by using a water depth of 1000 m
                  bare(k1) = bare(k1) + get_surface(stors(i), bl(k1) + 1d3)
               end if
            end do
         end if

         do n = ndx2D + 1, ndxi
            if (kcs(n) == 1) then
               call IN2Dflowcell(Xz(n), Yz(n), ja)
               if (ja >= 1) then
                  bare(n) = 0d0
               end if
            end if
         end do
         a1ini = sum(bare(1:ndxi))
      end if
      deallocate (sah)

      !  Check if there are any cells left that are not part of a mass balance area, and if we need an extra area.
      if (ti_mba > 0) then
         needextramba = 0
         do kk = 1, Ndxi
            if (mbadef(kk) == -999) then
               needextramba = 1
               exit
            end if
         end do

         if (jampi == 1) then
            ! check this among all domains (it could be that there are no remaing cels in this domain, while there are in other domains).
            call reduce_int_sum(needextramba, needextrambar)
            needextramba = needextrambar
         end if

         if (needextramba /= 0) then
            ! add the extra 'Unnamed' mass balance area, and assing the unassigned cells to this area.
            nomba = nomba + 1
            call realloc(mbaname, nomba, keepExisting=.true., fill="Unnamed")
            imba = nomba
            do kk = 1, Ndxi
               if (mbadef(kk) == -999) then
                  mbadef(kk) = imba
                  call getkbotktop(kk, kb, kt)
                  do k = kb, kb + kmxn(kk) - 1
                     mbadef(k) = imba
                  end do
               end if
            end do
         end if

         do kk = 1, Ndxi
            if (jampi == 1) then
               ! do not include ghost cells
               if (idomain(kk) /= my_rank) cycle
            end if
            mbadefdomain(kk) = mbadef(kk)
            call getkbotktop(kk, kb, kt)
            do k = kb, kb + kmxn(kk) - 1
               mbadefdomain(k) = mbadef(k)
            end do
         end do
      end if

      ! Copy NUMCONST to NUMCONST_MDU, before the user (optionally) adds tracers interactively
      NUMCONST_MDU = NUMCONST

      call initialize_lateraldata(numconst)

      ! Check if the model has any dams/dam breaks/gates/compound structures that lie across multiple partitions
      ! (needed to disable possibly invalid statistical output items)
      call check_model_has_structures_across_partitions

   end subroutine finalize

   !> Allocate and initialized atmosperic pressure variable(s)
   function allocate_patm(default_value) result(status)
      use m_wind, only: patm
      use m_cell_geometry, only: ndx
      use m_alloc, only: aerr
      use precision_basics, only: hp

      real(kind=hp), intent(in) :: default_value !< default atmospheric pressure value
      integer :: status

      status = 0
      if (.not. allocated(patm)) then
         allocate (patm(ndx), stat=status, source=default_value)
         call aerr('patm(ndx)', status, ndx)
      end if

   end function allocate_patm

end module fm_external_forcings
