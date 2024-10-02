subroutine swan_tot(n_swan_grids, n_flow_grids, wavedata, selectedtime)
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   use swan_flow_grid_maps
   use swan_input
   use update_waves
   use wave_data
   use buffer
   use meteo
   use m_deletehotfile
   use write_swan_datafile, only: write_swan_file
   !
   implicit none
!
! Global variables
!
   integer, intent(in) :: n_flow_grids
   integer, intent(in) :: n_swan_grids
   type(wave_data_type) :: wavedata
   integer, intent(in) :: selectedtime ! <=0: no time selected, >0: only compute for swan_run%timwav(selectedtime)
!
! Local variables
!
   integer :: count
   integer :: i_flow
   integer :: i_swan
   integer :: istat
   integer :: itide
   integer :: itidewrite
   integer :: lunhot
   integer :: offset
   real(fp) :: wave_timezone
   real(fp) :: wave_timmin
   real :: mult
   real(fp), dimension(:, :), pointer :: patm_fp
   real(fp), dimension(:, :), pointer :: windu_fp
   real(fp), dimension(:, :), pointer :: windv_fp
   logical :: DataFromPreviousTimestep
   logical :: DeleteSWANFile
   real(fp), dimension(:, :), pointer :: ice_frac_fp
   real(fp), dimension(:, :), pointer :: floe_dia_fp
   real(sp), dimension(:,:), allocatable :: tempveg
   logical :: extr_var1
   logical :: extr_var2
   logical :: sumvars
   logical :: success
   logical :: exists
   character(256) :: fname
   character(15), external :: datetime_to_string
   character(15) :: refdstr
   character(500) :: message
   type(swan_dom), pointer :: dom
!
!! executable statements -------------------------------------------------------
!
   !
   ! check to see if a wave map should be written
   !
   if (wavedata%mode == stand_alone) then
      call setwrite_wavm(wavedata%output, .true.)
   else
      if (wavedata%time%timsec >= wavedata%output%nexttim) then
         call setwrite_wavm(wavedata%output, .true.)
         call setnexttim(wavedata%output, wavedata%time%timsec + swan_run%wavm_write_interval * 60.0)
      else
         call setwrite_wavm(wavedata%output, .false.)
      end if
      !
      ! Keep the hotfile when:
      !   current time >= nextint
      !   int2keephotfile is specified
      !
   end if
   do itide = 1, swan_run%nttide
      if (selectedtime > 0 .and. itide /= selectedtime) cycle
      !
      call setcalculationcount(wavedata%time, wavedata%time%calccount + 1)
      if (wavedata%output%write_wavm) then
         call setoutputcount(wavedata%output, wavedata%output%count + 1)
      end if
      if (wavedata%time%calccount == 1 .and. swan_run%modsim == 3 .and. swan_run%inputtemplatefile == '') then
         !
         ! SWANFile is going to contain two datasets: from tstart and tend
         ! Output from tend will always be written
         ! Output from tstart will only be written when calccount=1
         ! In this case: increase output%count one more (indexing the tend field)
         !               on writing the tstart field: decrease output%count, write field and increase output%count
         ! This is necessary, because the increase of output%count must be done outside the "do i_swan"-loop
         !
         call setoutputcount(wavedata%output, wavedata%output%count + 1)
      end if
      !
      ! Set time in case of standalone run
      !
      if (wavedata%mode == stand_alone) call settimmin(wavedata%time, real(swan_run%timwav(itide), hp), swan_run%modsim, swan_run%nonstat_interval)
      !
      ! Update wave and wind conditions
      !
      call update_wavecond(swan_run, wavedata%time)
      !
      ! Start loop over SWAN grids
      !
      refdstr = datetime_to_string(wavedata%time%refdate, 0.0_hp)
      write (*, '(a,f15.3,a,a)') '  Start loop over SWAN grids, time = ', wavedata%time%timmin, ' minutes since ', trim(refdstr)
      do i_swan = 1, n_swan_grids
         dom => swan_run%dom(i_swan)

         write (*, '(a)') '  Allocate input fields'
         call alloc_input_fields(swan_grids(i_swan), swan_input_fields, wavedata%mode, dom%ice)
         call init_input_fields(swan_input_fields, swan_run, itide)

         if (dom%curvibot == 1) then
            write (*, '(a)') '  Allocate and read SWAN depth'
            call get_swan_depth(swan_input_fields, dom%botfil)
         end if
         !
         ! Vegetation map
         if (dom%vegetation == 1) then
            if (dom%vegfil /= '') then
               write (*, '(a)') '  Allocate and read vegetation map'
               call get_vegi_map(swan_input_fields, dom%vegfil)
            else
               swan_input_fields%veg = dom%veg_nstems
            end if
         end if
         ! If flow results are used
         !
         if (swan_run%useflowdata) then
            do i_flow = 1, n_flow_grids
               write (*, '(a,i0,a)') '  Get flow fields, domain ', i_flow, ' :'
               call get_flow_fields(i_flow, i_swan, swan_input_fields, flow_grids(i_flow), swan_grids(i_swan), &
                                   & flow2swan_maps(i_swan, i_flow), wavedata, &
                                   & swan_run, dom%flowVelocityType)
            end do
         end if
         !
         ! Get meteo data from file?
         ! Only when using space varying meteo data and
         ! when meteo data has not been obtained from FLOW
         !
         if (dom%n_extforces > 0) then
            wave_timezone = real(swan_run%tzone, fp)
            wave_timmin = real(wavedata%time%timmin, fp)
            success = meteoupdate(swan_grids(i_swan)%grid_name, wavedata%time%refdate, wave_timezone, wave_timmin)
            call checkmeteoresult_wave(success)
            if (dom%qextnd(q_wind) == 0) then
               !
               ! update windu array
               !
               call get_buffer(windu_fp, swan_input_fields%mmax, swan_input_fields%nmax)
               success = getmeteoval(swan_grids(i_swan)%grid_name, 'windu', wave_timmin, &
                                   & 1, 1, 1, swan_input_fields%nmax, &
                                   &      1, swan_input_fields%mmax, &
                                   & windu_fp, 0)
               call checkmeteoresult_wave(success)
               swan_input_fields%windu = real(windu_fp, sp)
               !
               ! update windv array
               !
               call get_buffer(windv_fp, swan_input_fields%mmax, swan_input_fields%nmax)
               success = getmeteoval(swan_grids(i_swan)%grid_name, 'windv', wave_timmin, &
                                   & 1, 1, 1, swan_input_fields%nmax, &
                                   &        1, swan_input_fields%mmax, &
                                   & windv_fp, 0)
               call checkmeteoresult_wave(success)
               swan_input_fields%windv = real(windv_fp, sp)
               !
               ! update patm array
               !
               !call get_buffer(patm_fp, swan_input_fields%mmax, swan_input_fields%nmax)
               !success = getmeteoval(swan_grids(i_swan)%grid_name, 'patm'      , wave_timmin, &
               !                    & 1, 1, 1, swan_input_fields%nmax   , &
               !                    &       1, swan_input_fields%mmax   , &
               !                    & patm_fp   , 0     )
               !call checkmeteoresult_wave(success)
               !swan_input_fields%patm = real(patm_fp, sp)
            end if
            if (dom%ice > 0) then
               !
               ! update ice_frac array
               !
               call get_buffer(ice_frac_fp, swan_input_fields%mmax, swan_input_fields%nmax)
               success = getmeteoval(swan_grids(i_swan)%grid_name, 'sea_ice_area_fraction', wave_timmin, &
                                   & 1, 1, 1, swan_input_fields%nmax, &
                                   &      1, swan_input_fields%mmax, &
                                   & ice_frac_fp, 0)
               call checkmeteoresult_wave(success)
               swan_input_fields%ice_frac = real(ice_frac_fp, sp)
               !
               ! update floe_dia array
               !
               call get_buffer(floe_dia_fp, swan_input_fields%mmax, swan_input_fields%nmax)
               success = getmeteoval(swan_grids(i_swan)%grid_name, 'floe_diameter', wave_timmin, &
                                   & 1, 1, 1, swan_input_fields%nmax, &
                                   &      1, swan_input_fields%mmax, &
                                   & floe_dia_fp, 0)
               call checkmeteoresult_wave(success)
               swan_input_fields%floe_dia = real(floe_dia_fp, sp)
            end if
            !
            ! Deallocate buffer
            !
            call dealloc_buffer()
         end if
         !
         ! Extend FLOW data on this SWAN grid?
         !
         if (dom%curvibot == 1) then
            !
            ! Write SWAN depth file
            !
            write (*, '(a)') '  Write SWAN depth file'
            sumvars = .true.
            extr_var1 = dom%qextnd(q_bath) == 2
            extr_var2 = dom%qextnd(q_wl) == 2
            call write_swan_file(swan_input_fields%dps, &
                                & swan_input_fields%s1, &
                                & swan_input_fields%mmax, &
                                & swan_input_fields%nmax, &
                                & swan_grids(i_swan)%covered, &
                                & 'BOTNOW', extr_var1, extr_var2, &
                                & sumvars, swan_run%depmin)
         end if
         if (dom%qextnd(q_cur) > 0 .or. swan_run%swuvi) then
            !
            ! Write SWAN velocity file
            !
            write (*, '(a)') '  Write SWAN velocity file'
            sumvars = .false.
            extr_var1 = dom%qextnd(q_cur) == 2
            extr_var2 = dom%qextnd(q_cur) == 2
            call write_swan_file(swan_input_fields%u1, &
                                & swan_input_fields%v1, &
                                & swan_input_fields%mmax, &
                                & swan_input_fields%nmax, &
                                & swan_grids(i_swan)%covered, &
                                & 'CURNOW', extr_var1, extr_var2, &
                                & sumvars)
         end if
         if (dom%qextnd(q_wind) > 0 .or. dom%n_extforces > 0) then
            !
            ! Write SWAN wind file
            !
            write (*, '(a)') '  Write SWAN wind file'
            sumvars = .false.
            extr_var1 = dom%qextnd(q_wind) == 2
            extr_var2 = dom%qextnd(q_wind) == 2
            call write_swan_file(swan_input_fields%windu, &
                                & swan_input_fields%windv, &
                                & swan_input_fields%mmax, &
                                & swan_input_fields%nmax, &
                                & swan_grids(i_swan)%covered, &
                                & 'WNDNOW', extr_var1, extr_var2, &
                                & sumvars)
         end if
         if (wavedata%mode == flow_mud_online) then
            !
            ! Write SWAN mud file
            ! Never extend!
            !
            write (*, '(a)') '  Write SWAN mud file'
            sumvars = .true.
            extr_var1 = .false.
            extr_var2 = .false.
            call write_swan_file(swan_input_fields%dpsmud, &
                                & swan_input_fields%s1mud, &
                                & swan_input_fields%mmax, &
                                & swan_input_fields%nmax, &
                                & swan_grids(i_swan)%covered, &
                                & 'MUDNOW', extr_var1, extr_var2, &
                                & sumvars, 0.0)
         end if
         mult = 1.0
         if (dom%vegetation == 2 .or. (dom%vegetation == 1 .and. dom%vegfil /= '')) then
            !
            ! Write Vegetation map file
            !
            write (*, '(a)') '  Write Vegetation map file'
            !
            ! mult needed for writing correct stem density to output
            ! in SWAN, the applied density is veg*nstems
            if (dom%vegetation == 1) then
               mult = dom%veg_nstems
            end if
            !
            extr_var1 = .false.
            call write_swan_file(swan_input_fields%veg, &
                                & swan_input_fields%mmax, &
                                & swan_input_fields%nmax, &
                                & swan_grids(i_swan)%covered, &
                                & 'VEGNOW', extr_var1)
         end if
         if (swan_run%icedamp == 2 .and. dom%ice == 1) then
            !
            ! Write ice fraction map file
            !
            write (*, '(a)') '  Write SWAN ice fraction map file'
            extr_var1 = .false. ! dom%qextnd(q_ice) == 2
            call write_swan_file(swan_input_fields%ice_frac, &
                                & swan_input_fields%mmax, &
                                & swan_input_fields%nmax, &
                                & swan_grids(i_swan)%covered, &
                                & 'AICENOW', extr_var1, 0.0)
         end if

         ! Update SWAN wind and wave conditions in SWAN input file based on wavecon time-series file

         ! Write SWAN input
         write (*, '(a)') '  Write SWAN input'
         dom%curlif = swan_grids(i_swan)%tmp_name

         call write_swan_input(swan_run, itide, wavedata%time%calccount, i_swan, swan_grids(i_swan)%xymiss, wavedata)

         ! The following commented code was used for a special version
         ! - to be implemented in a more constructive way
         ! Prepare input files for Part file runs
         !write(sfile,'(a4,i3.3,a4)') 'part', istep, '.txt'
         !call cp_file(sfile,'partmiod.txt','copy',nuerr)

         ! Run SWAN
         write (*, '(a)') '<<Run SWAN...'
         call run_swan(swan_run%casl)

         ! Allocate swan output fields
         write (*, '(a)') '  Allocate output fields'
         if (allocated(swan_run%add_out_names)) then
            swan_output_fields%n_outpars = size(swan_run%add_out_names)
         else
            swan_output_fields%n_outpars = 0
         end if
         call alloc_output_fields(swan_grids(i_swan), swan_output_fields)
         if (allocated(swan_run%add_out_names)) then
            swan_output_fields%add_out_names = swan_run%add_out_names
         end if
         !
         ! Read SWAN output
         !
         write (*, '(a)') '  Read SWAN output'
         offset = 0
         if (wavedata%time%calccount == 1 .and. swan_run%modsim == 3 .and. swan_run%inputtemplatefile == '') then
            ! SWANFile contains two datasets: from tstart and tend
            ! First read the first dataset
            ! This must be placed in output%count-1
            !
            DeleteSWANFile = .false.
            call read_swan_output(swan_output_fields, swan_run, offset, DeleteSWANFile)
            if (dom%cgnum .and. wavedata%output%write_wavm) then
               call setoutputcount(wavedata%output, wavedata%output%count - 1)
               !
               ! Write output to WAVE map file
               !
               write (*, '(a,i10,a,f15.3)') '  Write WAVE map file, nest ', i_swan, ' time ', wavedata%time%timmin
               DataFromPreviousTimestep = .true.
               call write_wave_map(swan_grids(i_swan), swan_output_fields, swan_input_fields,&
                                  & n_swan_grids, wavedata, swan_run%casl, DataFromPreviousTimestep, &
                                  & swan_run%gamma0, swan_run%output_ice)
               if (swan_run%swmapwritenetcdf) then
                  if (allocated(tempveg)) deallocate(tempveg)
                  allocate(tempveg(swan_input_fields%mmax, swan_input_fields%nmax))
                  tempveg = swan_input_fields%veg * mult
                  write (*, '(a,i10,a,f10.3)') '  Write WAVE NetCDF map file, nest ', i_swan, ' time ', wavedata%time%timmin
                  call write_wave_map_netcdf(swan_grids(i_swan), swan_output_fields, swan_input_fields, &
                                     & n_swan_grids, wavedata, swan_run%casl, DataFromPreviousTimestep, &
                                     & swan_run%netcdf_sp, swan_input_fields%mmax, swan_input_fields%nmax, tempveg, swan_run%output_ice, swan_run%output_veg)
               end if
               call setoutputcount(wavedata%output, wavedata%output%count + 1)
            end if
            offset = 1
         end if
         DeleteSWANFile = .true.
         call read_swan_output(swan_output_fields, swan_run, offset, DeleteSWANFile)
         !
         if (swan_run%icedamp == 1 .and. dom%ice > 0) then
            call postprocess_ice(swan_input_fields%mmax, swan_input_fields%nmax, &
                               & swan_input_fields%ice_frac, swan_input_fields%floe_dia, &
                               & swan_output_fields%hs)
         end if
         !
         if (swan_run%swwav) then
            !
            ! For each com-file (flow domain)
            !
            do i_flow = 1, n_flow_grids
               !
               ! Map WAVE parameters to FLOW grid
               !
               write (*, '(a,i10)') '  Map WAVE parameters to FLOW grid ', i_flow
               call map_swan_output(swan_output_fields,            &
                           &        flow_output_fields(i_flow),    &
                           &        swan2flow_maps(i_swan, i_flow), &
                           &        flow_grids(i_flow))
            end do
         end if
         if (dom%cgnum .and. wavedata%output%write_wavm) then
            !
            ! Write output to WAVE map file
            !
            write (*, '(a,i10,a,f15.3)') '  Write WAVE map file, nest ', i_swan, ' time ', wavedata%time%timmin
            DataFromPreviousTimestep = .false.
            call write_wave_map(swan_grids(i_swan), swan_output_fields, swan_input_fields, &
                               & n_swan_grids, wavedata, swan_run%casl, DataFromPreviousTimestep, &
                               & swan_run%gamma0, swan_run%output_ice)
            if (swan_run%swmapwritenetcdf) then
               if (allocated(tempveg)) deallocate(tempveg)
               allocate(tempveg(swan_input_fields%mmax, swan_input_fields%nmax))
               tempveg = swan_input_fields%veg * mult
               write (*, '(a,i10,a,f10.3)') '  Write WAVE NetCDF map file, nest ', i_swan, ' time ', wavedata%time%timmin
               call write_wave_map_netcdf(swan_grids(i_swan), swan_output_fields, swan_input_fields, &
                                  & n_swan_grids, wavedata, swan_run%casl, DataFromPreviousTimestep, &
                                  & swan_run%netcdf_sp, swan_input_fields%mmax, swan_input_fields%nmax, tempveg, swan_run%output_ice, swan_run%output_veg)
            end if
         end if
         if (swan_run%output_points .and. swan_run%output_table) then
            write (*, '(a,i10,a,f10.3)') '  Write WAVE NetCDF his file, nest ', i_swan, ' time ', wavedata%time%timmin
            call write_wave_his_netcdf(swan_grids(i_swan), swan_output_fields, n_swan_grids, i_swan, &
                               & wavedata)
         end if

         call dealloc_input_fields(swan_input_fields, wavedata%mode)
         call dealloc_output_fields(swan_output_fields)
         !
         if (deletehotfile(wavedata)) then
            !
            ! The hotfile related to "usehottime" has been used and can now be deleted
            !
            write (fname, '(a,i0,5a)') 'hot_', i_swan, '_', trim(swan_run%usehottime(1:8)), '_', trim(swan_run%usehottime(10:15)), '.nc'
            inquire (file=trim(fname), exist=exists)
            if (exists) then
               open (newunit=lunhot, file=trim(fname))
               close (lunhot, status='delete')
            end if
            !
            ! When using SWAN-MPI, the names of the hot-files generated are extended with -001, -002 etc
            ! Keep removing them until there is no one left
            !
            count = 0
            do
               count = count + 1
               write (fname, '(a,i0,5a,i3.3,a)') 'hot_', i_swan, '_', trim(swan_run%usehottime(1:8)), '_', trim(swan_run%usehottime(10:15)), '-', count, '.nc'

               inquire (file=trim(fname), exist=exists)
               if (exists) then
                  open (newunit=lunhot, file=trim(fname))
                  close (lunhot, status='delete')
               else
                  !
                  ! exit do loop
                  !
                  exit
               end if
            end do
         end if

         write (*, '(a)') '  Deallocate input fields'
         call dealloc_input_fields(swan_input_fields, wavedata%mode, dom%ice)
      end do ! nested swan grids
      !
      ! After all hotfiles are handled correctly for each i_swan:
      ! Next time, use the last written hotfile
      !
      swan_run%usehottime = swan_run%writehottime
      !
      ! gl THINK THIS CHECK SHOULD BE AROUND ALL WRITING TO THE COM FILE
      !
      if (swan_run%swwav) then
         do i_flow = 1, n_flow_grids
            if (swan_run%flowgridfile == ' ') then
               !
               ! Convert vector fields to curvilinear directions
               !
               write (*, '(a,i10)') '  Convert vector field ', i_flow
               call wave2flow(flow_output_fields(i_flow), flow_grids(i_flow))
            end if
            !
            ! Convert some parameters to communication parameters
            !
            write (*, '(a)') '  Convert parameters'
            call wave2com(flow_output_fields(i_flow), swan_run)
            !
            ! Write to communication file(s)
            !
            write (*, '(a)') '  Write to com-file'
            itidewrite = itide
            if (swan_run%append_com) itidewrite = -1
            call put_wave_fields(flow_grids(i_flow), flow_output_fields(i_flow), itidewrite, &
                                & wavedata, swan_run%swflux, swan_run%flowgridfile, &
                                & swan_run%netcdf_sp)
         end do
         ! Initially comcount = 0
         ! After writing the first data set to (all) the comfile(s), comcount must be increased
         ! Always write to field 1, unless append_com is true
         if (swan_run%append_com) then
            ! comcount = 0: The first fields have just been written; next time: comcount = 2
            if (wavedata%output%comcount == 0) then
               wavedata%output%comcount = 2
            else
               wavedata%output%comcount = wavedata%output%comcount + 1
            end if
         else
            wavedata%output%comcount = 1
         end if
      end if
   end do ! time steps
end subroutine swan_tot
