!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_sedtrails_netcdf
   use m_sedtrails_data
   use unstruc_netcdf
   use netcdf_utils, only: ncu_ensure_define_mode, ncu_ensure_data_mode, ncu_restore_mode

   implicit none

   type, private :: t_profile_ids
      integer :: velocity(3)
      integer :: layer, height, height_bounds, duration, time_bounds
   end type t_profile_ids

   character(len=23), parameter, private :: VELOCITY_NAMES(3) = &
      [character(len=23) :: 'sea_water_x_velocity_3d', 'sea_water_y_velocity_3d', 'sea_water_z_velocity_3d']
   integer, save, private :: sedtrails_ncid = 0
   character(len=:), allocatable, save, private :: sedtrails_filename

   private :: sedtrails_unc_read_net_ugrid, &
              unc_write_sedtrails_filepointer, &
              sedtrails_write_nc, &
              sedtrails_unc_write_flowgeom_filepointer

contains

   !> Write the next scheduled record and reset the completed averaging window.
   subroutine sedtrails_write_stats(tim, status)
      use precision, only: dp
      use m_flowparameters, only: EPS10
      use m_flowtimes, only: ti_st, ti_sts, ti_ste, tstop_user, time_st
      use precision_basics, only: comparereal
      use m_sedtrails_stats, only: reset_sedtrails_stats, is_dtint
      use m_partitioninfo, only: jampi, reduce_int1_max
      use dfm_error, only: DFM_NOERR, DFM_GENERICERROR

      implicit none

      real(kind=dp), intent(in) :: tim !< End time of the latest solver step.
      integer, intent(out) :: status !< D-Flow FM result code.

      integer :: ierr
      real(kind=dp) :: tem_dif

      status = DFM_NOERR
      if (ti_st > 0.0_dp .and. is_dtint > 0.0_dp) then
         if (comparereal(tim, time_st, EPS10) >= 0) then
            call sedtrails_write_nc(time_st, ierr)
            if (ierr /= nf90_noerr) then
               status = DFM_GENERICERROR
            end if
            if (jampi > 0) then
               call reduce_int1_max(status)
            end if
            if (status /= DFM_NOERR) then
               return
            end if
            call reset_sedtrails_stats()
            if (comparereal(time_st, ti_ste, EPS10) == 0) then
               time_st = tstop_user + 1.0_dp
            else
               tem_dif = (tim - ti_sts) / ti_st
               time_st = max(ti_sts + (floor(tem_dif + 0.001_dp) + 1) * ti_st, ti_sts)

               if (comparereal(time_st, ti_ste, EPS10) == 1) then
                  ! next time_map would be beyond end of map-window, write one last map exactly at that end.
                  time_st = ti_ste
               end if
            end if
         end if
      end if

   end subroutine sedtrails_write_stats

   !> Open the rank-local file on demand and write a complete record.
   subroutine sedtrails_write_nc(tim, ierr)
      use precision, only: dp
      use netcdf
      use m_flowtimes, only: it_st
      use unstruc_files, only: defaultFilename
      use messagehandling, only: mess, LEVEL_ERROR

      real(kind=dp), intent(in) :: tim !< Scheduled record time.
      integer, intent(out) :: ierr !< NetCDF result code.

      character(len=NF90_MAX_NAME) :: field
      character(len=24) :: record

      ierr = nf90_noerr
      if (numk <= 0) then
         return
      end if
      if (sedtrails_ncid == 0) then
         sedtrails_filename = trim(defaultFilename('sedtrails'))
         ierr = unc_create(sedtrails_filename, 0, sedtrails_ncid)
         if (ierr /= nf90_noerr) then
            sedtrails_ncid = 0
            call mess(LEVEL_ERROR, 'Cannot create SedTRAILS output '//sedtrails_filename//': '//trim(nf90_strerror(ierr)))
            return
         end if
      end if

      call unc_write_sedtrails_filepointer(sedtrails_ncid, tim, ierr, field)
      if (ierr == nf90_noerr .and. unc_noforcedflush == 0) then
         field = 'file synchronization'
         ierr = nf90_sync(sedtrails_ncid)
      end if
      if (ierr /= nf90_noerr) then
         write (record, '(i0)') it_st + 1
         call mess(LEVEL_ERROR, 'SedTRAILS '//sedtrails_filename//' record '//trim(record)//' ('//trim(field)// &
                   '): '//trim(nf90_strerror(ierr)))
         return
      end if
      it_st = it_st + 1
   end subroutine sedtrails_write_nc

   !> Close the current model's SedTRAILS output.
   subroutine close_sedtrails_output()
      use messagehandling, only: mess, LEVEL_WARN

      integer :: ierr

      if (sedtrails_ncid /= 0) then
         ierr = unc_close(sedtrails_ncid)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'Cannot close SedTRAILS output '//sedtrails_filename//': '//trim(nf90_strerror(ierr)))
         end if
         sedtrails_ncid = 0
      end if
      if (allocated(sedtrails_filename)) then
         deallocate (sedtrails_filename)
      end if
   end subroutine close_sedtrails_output

!> Reads the net data from a NetCDF file.
!! Processing is done elsewhere.
   subroutine sedtrails_unc_read_net_ugrid(filename, numk_keep, numk_read, ierr)
      use precision, only: dp
      use m_sedtrails_data
      use m_sedtrails_network, only: sedtrails_increasenetwork
      use m_save_ugrid_state
      use io_netcdf
      use netcdf
      use netcdf_utils, only: ncu_get_att
      use m_sferic
      use m_missing
      use MessageHandling
      use dfm_error
      use m_alloc

      implicit none

      character(len=*), intent(in) :: filename !< Name of NetCDF file.
      integer, intent(inout) :: numk_keep !< Number of netnodes to keep in existing net (0 to replace all).
      integer, intent(out) :: numk_read !< Number of new netnodes read from file.
      integer, intent(out) :: ierr !< Return status (NetCDF operations)

      integer :: ioncid, iconvtype, start_index, networkIndex
      integer :: im, nmesh, i, numk_last
      integer :: ncid
      real(kind=dp) :: convversion
      type(t_ug_meshgeom) :: meshgeom

      character(len=:), allocatable :: tmpstring
      logical :: need_edgelengths, includeArrays

      numk_read = 0
      start_index = 1
      numk_last = numk_keep
      includeArrays = .true.
      networkIndex = 0

      allocate (character(len=0) :: tmpstring)
      ierr = ionc_open(filename, NF90_NOWRITE, ioncid, iconvtype, convversion)

      if (ierr /= ionc_noerr .or. iconvtype /= IONC_CONV_UGRID .or. convversion < 1.0) then
         call mess(LEVEL_ERROR, 'sedtrails_unc_read_net_ugrid:: net file '''//trim(filename)//''' is not UGRID. Save network file with cell info.')
         ierr = DFM_EFILEFORMAT
         goto 999
      end if

      ierr = ionc_get_ncid(ioncid, ncid)
      tmpstring = ''
      ierr = ncu_get_att(ncid, nf90_global, 'Conventions', tmpstring)
      if (ierr == NF90_ENOTATT) then
         call mess(LEVEL_DEBUG, 'sedtrails_unc_read_net_ugrid::No NetCDF Conventions found. Defaulting to current format (>= "CF-1.8 UGRID-1.0 Deltares-0.10") for '''//trim(filename)//'''.')
      end if
      deallocate (tmpstring)

      ierr = ionc_get_coordinate_reference_system(ioncid, crs)
      if (ierr /= ionc_noerr) then
         call mess(LEVEL_WARN, 'sedtrails_unc_read_net_ugrid::ionc_get_coordinate_system: No epsg_code found in UGRID net file '''//trim(filename)//'''.')
         goto 999
      end if
      !
      select case (crs%epsg_code)
      case (4326) ! WGS84
         jsferic = 1
      case default
         jsferic = 0
         jasfer3D = 0
      end select
      !
      ! Prepare for multiple (partial) meshes
      !
      ierr = ionc_get_mesh_count(ioncid, nmesh)
      if (ierr /= ionc_noerr) then
         call mess(LEVEL_WARN, 'sedtrails_unc_read_net_ugrid::: No grids found in UGRID net file '''//trim(filename)//'''.')
         goto 999
      end if

      !------------------------------------------------------------!
      ! meshes
      !------------------------------------------------------------!
      do im = 1, nmesh

         ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom)

         if (meshgeom%dim == 2) then
            !Else 2d/3d mesh
            if (meshgeom%numnode < 0 .or. meshgeom%numface < 0) then
               cycle
            end if
            ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom, start_index, includeArrays)
            mesh2dname = meshgeom%meshname
         else
            ! Only support 2D grid
            write (msgbuf, '(a,i0,a,i0,a)') 'sedtrails_unc_read_net_ugrid: unsupported topology dimension ', meshgeom%dim, &
               ' in file '''//trim(filename)//' for mesh #', im, '.'
            call warn_flush()
            cycle
         end if

         need_edgelengths = .false. ! Either from a previous meshgeom, or now for the first time.

         !increasenetw
         call sedtrails_increasenetwork(numk_last + meshgeom%numnode) ! increases xk, yk, kn, optionally dxe
         if (meshgeom%dim == 2) then ! always true, see above
            ! 2d
            ierr = ionc_get_node_coordinates(ioncid, im, xk(numk_last + 1:numk_last + meshgeom%numnode), yk(numk_last + 1:numk_last + meshgeom%numnode))
         end if

         if (ierr /= ionc_noerr) then
            write (msgbuf, '(a,i0,a)') 'sedtrails_unc_read_net_ugrid: could not read x/y node coordinates from mesh #', im, ' in ugrid net file '''//trim(filename)//'''.'
            call warn_flush()
            goto 999
         end if

         ierr = ionc_get_ncid(ioncid, ncid)
         if (ierr /= ionc_noerr) then
            write (msgbuf, '(a,i0,a)') 'sedtrails_unc_read_net_ugrid: Could not get direct access to UGRID NetCDF net file '''//trim(filename)//'''.'
            call warn_flush()
            goto 999
         end if

         numk_read = numk_read + meshgeom%numnode
         numk_last = numk_last + meshgeom%numnode

      end do

      call realloc(zk, numk_read, keepExisting=.false., fill=0.0_dp)

      ! Success
888   continue
      ierr = ionc_close(ioncid)
      ierr = dfm_noerr
      return

999   continue
      ! Some error occurred (error code previously set)
      ! Try to close+cleanup the data set anyway.
      i = ionc_close(ioncid) ! Don't overwrite actual ierr.

   end subroutine sedtrails_unc_read_net_ugrid

!> Reads the net data from a NetCDF file.
!! Processing is done elsewhere.
   subroutine sedtrails_unc_read_net(filename, numk_keep, numk_read, ierr)
      use m_sedtrails_data
      use m_sferic
      use m_missing
      use dfm_error

      implicit none

      character(len=*), intent(in) :: filename !< Name of NetCDF file.
      integer, intent(inout) :: numk_keep !< Number of netnodes to keep in existing net.
      integer, intent(out) :: numk_read !< Number of new netnodes read from file.
      integer, intent(out) :: ierr !< Return status (NetCDF operations)

      call mess(LEVEL_INFO, 'sedtrails_unc_read_net::Reading net data...')
      !
      ! Try and read as new UGRID NetCDF format
      !
      call sedtrails_unc_read_net_ugrid(filename, numk_keep, numk_read, ierr)
      if (ierr == dfm_noerr) then
         ! UGRID successfully read, we're done.
         return
      else
         ! No UGRID, error.
         call mess(LEVEL_ERROR, 'sedtrails_unc_read_net::Could not read '''//trim(filename)//'''')
         return
      end if

   end subroutine sedtrails_unc_read_net

!> Writes the unstructured sedtrails geometry to an already opened netCDF dataset.
   subroutine sedtrails_unc_write_flowgeom_filepointer(igeomfile)
      use m_sedtrails_data
      use m_sferic
      use m_missing
      use netcdf
      use m_partitioninfo, only: jampi

      implicit none

      integer, intent(in) :: igeomfile
      integer :: ndxndxi
      integer :: ierr
      integer :: id_flowelemdim, &
                 id_flowelemxcc, id_flowelemycc, &
                 id_flowelemdomain, id_flowelemglobalnr

      logical :: jaInDefine

      jaInDefine = 0

      if (numk <= 0) then
         call mess(LEVEL_WARN, 'sedtrails_unc_write_flowgeom_filepointer :: No flow elements in model, will not write flow geometry.')
         return
      end if

      ndxndxi = numk

      ! Put dataset in define mode (possibly again) to add dimensions and variables.
      ierr = ncu_ensure_define_mode(igeomfile, jaInDefine)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'sedtrails_unc_write_flowgeom_filepointer::Could not put header in sedtrails geometry file.')
         call check_error(ierr)
         return
      end if
      ierr = nf90_def_dim(igeomfile, 'nNodes', ndxndxi, id_flowelemdim)

      ! Net nodes
      ierr = nf90_def_var(igeomfile, 'net_xcc', nf90_double, id_flowelemdim, id_flowelemxcc)
      ierr = nf90_def_var(igeomfile, 'net_ycc', nf90_double, id_flowelemdim, id_flowelemycc)

      ierr = unc_addcoordatts(igeomfile, id_flowelemxcc, id_flowelemycc, jsferic)
      ierr = nf90_put_att(igeomfile, id_flowelemxcc, 'long_name', 'x-coordinate of sedtrails grid corner')
      ierr = nf90_put_att(igeomfile, id_flowelemycc, 'long_name', 'y-coordinate of sedtrails grid corner')

      ! Coordinate/grid mapping
      ierr = unc_addcoordmapping(igeomfile, jsferic)

      !   domain numbers
      if (jampi == 1) then
         ierr = nf90_def_var(igeomfile, 'FlowElemDomain', nf90_int, id_flowelemdim, id_flowelemdomain)
         ierr = nf90_put_att(igeomfile, id_flowelemdomain, 'long_name', 'domain number of sedtrails grid corner')
         ierr = nf90_def_var(igeomfile, 'FlowElemGlobalNr', nf90_int, id_flowelemdim, id_flowelemglobalnr)
         ierr = nf90_put_att(igeomfile, id_flowelemglobalnr, 'long_name', 'global sedtrails corner numbering')
      end if

      ierr = nf90_enddef(igeomfile)
      ! End of writing time-independent flow net data.

      ! -- Start data writing (time-independent data) ------------
      ! Flow cell cc coordinates
      ierr = nf90_put_var(igeomfile, id_flowelemxcc, xk(1:ndxndxi))
      ierr = nf90_put_var(igeomfile, id_flowelemycc, yk(1:ndxndxi))

      ! domain numbers
      if (jampi == 1) then
         ! flow cell domain number
         ierr = nf90_put_var(igeomfile, id_flowelemdomain, idomain(1:ndxndxi))
         ierr = nf90_put_var(igeomfile, id_flowelemglobalnr, iglobal_s(1:ndxndxi))
      end if

      ! Leave the dataset in the same mode as we got it.
      ierr = ncu_restore_mode(igeomfile, jaInDefine)

   end subroutine sedtrails_unc_write_flowgeom_filepointer

   subroutine unc_write_sedtrails_filepointer(imapfile, tim, ierr, field)
      use precision, only: dp
      use m_sedtrails_stats
      use m_alloc
      use m_flowtimes, only: Tudunitstr, it_st
      use m_fm_erosed
      use m_sediment, only: stm_included
      use m_flowparameters, only: jawave
      use m_waveconst

      implicit none

      integer, intent(in) :: imapfile
      real(kind=dp), intent(in) :: tim
      integer, intent(out) :: ierr
      character(len=*), intent(out) :: field

      ! Locals
      integer :: ndxndxi
      integer :: iid, k, l, ii
      integer :: itim
      integer, dimension(2) :: idims
      integer, dimension(2), save :: id_timedim, id_time, id_timestep, id_sbx, id_sby, id_ssx, id_ssy, id_ssc, &
                                     id_sedtotdim, id_flowelemdim, id_ucx, id_ucy, id_bl, id_hs, id_taus, id_tausmax, &
                                     id_ua, id_va
      real(kind=dp), allocatable :: work(:, :)
      integer, allocatable :: nodes(:)
      type(t_profile_ids), save :: profile_ids

      ! Define variables and write time-invariant data
      ierr = nf90_noerr
      iid = 1
      ndxndxi = numk

      ! Only write net and flow geometry data the first time, or for a separate map file.
      if (it_st == 0) then
         field = 'point geometry'
         call sedtrails_unc_write_flowgeom_filepointer(imapfile) ! Write standard net data as well
         ierr = nf90_inq_dimid(imapfile, 'nNodes', id_flowelemdim(iid))
         if (ierr /= nf90_noerr) then
            return
         end if

         ! Time
         ierr = nf90_def_dim(imapfile, 'time', nf90_unlimited, id_timedim(iid))
         if (ierr /= nf90_noerr) then
            return
         end if
         ierr = nf90_def_var(imapfile, 'time', nf90_double, id_timedim(iid), id_time(iid))
         if (ierr /= nf90_noerr) then
            return
         end if
         ierr = nf90_put_att(imapfile, id_time(iid), 'units', trim(Tudunitstr))
         if (ierr /= nf90_noerr) then
            return
         end if
         ierr = nf90_put_att(imapfile, id_time(iid), 'standard_name', 'time')
         if (ierr /= nf90_noerr) then
            return
         end if

         ! Size of latest timestep
         ierr = nf90_def_var(imapfile, 'timestep', nf90_double, id_timedim(iid), id_timestep(iid))
         if (ierr /= nf90_noerr) then
            return
         end if
         ierr = nf90_put_att(imapfile, id_timestep(iid), 'units', 'seconds')
         if (ierr /= nf90_noerr) then
            return
         end if
         ierr = nf90_put_att(imapfile, id_timestep(iid), 'standard_name', 'timestep')
         if (ierr /= nf90_noerr) then
            return
         end if

         idims(1) = id_flowelemdim(iid)
         idims(2) = id_timedim(iid)

         ! Variables that will always be written
         call definencvar(imapfile, id_bl(iid), nf90_double, idims, 'bedlevel', 'bed level', 'm', 'net_xcc net_ycc')

         if (trim(sedtrails_analysis) == 'flowvelocity' .or. trim(sedtrails_analysis) == 'all') then
            call definencvar(imapfile, id_ucx(iid), nf90_double, idims, 'sea_water_x_velocity', 'depth-averaged velocity on grid corner, x-component', 'm s-1', 'net_xcc net_ycc')
            call definencvar(imapfile, id_ucy(iid), nf90_double, idims, 'sea_water_y_velocity', 'depth-averaged velocity on grid corner, y-component', 'm s-1', 'net_xcc net_ycc')
         end if

         if ((trim(sedtrails_analysis) == 'transport' .or. trim(sedtrails_analysis) == 'all') .and. stm_included) then
            ! Fractions
            ierr = nf90_def_dim(imapfile, 'nSedTot', lsedtot, id_sedtotdim(iid))
            if (ierr /= nf90_noerr) then
               return
            end if
            !
            call definencvar(imapfile, id_sbx(iid), nf90_double, [id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)], 'bedload_x_comp', 'bedload transport on grid corner, x-component', 'kg m-1 s-1', 'net_xcc net_ycc')
            call definencvar(imapfile, id_sby(iid), nf90_double, [id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)], 'bedload_y_comp', 'bedload transport on grid corner, y-component', 'kg m-1 s-1', 'net_xcc net_ycc')
            call definencvar(imapfile, id_ssx(iid), nf90_double, [id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)], 'susload_x_comp', 'suspended transport on grid corner, x-component', 'kg m-1 s-1', 'net_xcc net_ycc')
            call definencvar(imapfile, id_ssy(iid), nf90_double, [id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)], 'susload_y_comp', 'suspended transport on grid corner, y-component', 'kg m-1 s-1', 'net_xcc net_ycc')
         end if

         if ((trim(sedtrails_analysis) == 'soulsby' .or. trim(sedtrails_analysis) == 'all') .and. stm_included) then
            ! Fractions
            ierr = nf90_inq_dimid(imapfile, 'nSedTot', id_sedtotdim(iid))
            if (ierr == nf90_ebaddim) then
               ierr = nf90_def_dim(imapfile, 'nSedTot', lsedtot, id_sedtotdim(iid)) ! only once
            end if
            if (ierr /= nf90_noerr) then
               return
            end if
            !
            call definencvar(imapfile, id_hs(iid), nf90_double, idims, 'waterdepth', 'water depth', 'm', 'net_xcc net_ycc')
            call definencvar(imapfile, id_taus(iid), nf90_double, idims, 'mean_bss_magnitude', 'mean bed shear stress magnitude', 'Pa', 'net_xcc net_ycc')
            call definencvar(imapfile, id_tausmax(iid), nf90_double, idims, 'max_bss_magnitude', 'max bed shear stress magnitude', 'Pa', 'net_xcc net_ycc')
            call definencvar(imapfile, id_ssc(iid), nf90_double, [id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)], 'suspended_sed_conc', 'depth-averaged suspended sediment concentration', 'kg m-3', 'net_xcc net_ycc')
            !
            if (jawave > NO_WAVES) then
               call definencvar(imapfile, id_ua(iid), nf90_double, idims, 'wave_nonlin_vel_x_comp', 'non-linear wave velocity contribution, x-component', 'm s-1', 'net_xcc net_ycc')
               call definencvar(imapfile, id_va(iid), nf90_double, idims, 'wave_nonlin_vel_y_comp', 'non-linear wave velocity contribution, y-component', 'm s-1', 'net_xcc net_ycc')
            end if
         end if
         !
         if (sedtrails_profiles_enabled()) then
            call define_sedtrails_profiles(imapfile, id_flowelemdim(iid), id_timedim(iid), id_time(iid), &
                                           Tudunitstr, profile_ids, ierr, field)
            if (ierr /= nf90_noerr) then
               return
            end if
         end if
         field = 'end of NetCDF definitions'
         ierr = nf90_enddef(imapfile)
         if (ierr /= nf90_noerr) then
            return
         end if
      end if

      ! interpolate and write variables
      itim = it_st + 1

      ! time
      field = 'time'
      ierr = nf90_put_var(imapfile, id_time(iid), tim, [itim])
      if (ierr /= nf90_noerr) then
         return
      end if
      field = 'timestep'
      ierr = nf90_put_var(imapfile, id_timestep(iid), is_dtint, [itim])
      if (ierr /= nf90_noerr) then
         return
      end if

      ! analysis:
      if (stm_included) then
         call realloc(work, [numk, lsedtot], keepexisting=.false., fill=0.0_dp)
      else
         call realloc(work, [numk, 1], keepexisting=.false., fill=0.0_dp)
      end if

      ! bottom level
      do k = 1, numk
         nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
         work(k, 1) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_BL, st_ind(nodes, k), 1))
      end do
      work = work / is_dtint
      field = 'bedlevel'
      ierr = nf90_put_var(imapfile, id_bl(iid), work(1:numk, 1), [1, itim], [ndxndxi, 1])
      if (ierr /= nf90_noerr) then
         return
      end if

      ! 'flowvelocity'
      if ((trim(sedtrails_analysis) == 'flowvelocity' .or. trim(sedtrails_analysis) == 'all')) then
         do k = 1, numk
            nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
            work(k, 1) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_UCX, st_ind(nodes, k), 1))
         end do
         work = work / is_dtint
         field = 'sea_water_x_velocity'
         ierr = nf90_put_var(imapfile, id_ucx(iid), work(1:numk, 1), [1, itim], [ndxndxi, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
         !
         do k = 1, numk
            nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
            work(k, 1) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_UCY, st_ind(nodes, k), 1))
         end do
         work = work / is_dtint
         field = 'sea_water_y_velocity'
         ierr = nf90_put_var(imapfile, id_ucy(iid), work(1:numk, 1), [1, itim], [ndxndxi, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
      end if

      !'transport'
      if ((trim(sedtrails_analysis) == 'transport' .or. trim(sedtrails_analysis) == 'all') .and. stm_included) then
         do l = 1, lsedtot
            do k = 1, numk
               nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
               work(k, l) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_SBX, st_ind(nodes, k), l))
            end do
         end do
         work = work / is_dtint
         field = 'bedload_x_comp'
         ierr = nf90_put_var(imapfile, id_sbx(iid), work(1:numk, 1:lsedtot), [1, 1, itim], [ndxndxi, lsedtot, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
         !
         do l = 1, lsedtot
            do k = 1, numk
               nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
               work(k, l) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_SBY, st_ind(nodes, k), l))
            end do
         end do
         work = work / is_dtint
         field = 'bedload_y_comp'
         ierr = nf90_put_var(imapfile, id_sby(iid), work(1:numk, 1:lsedtot), [1, 1, itim], [ndxndxi, lsedtot, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
         !
         do l = 1, lsedtot
            do k = 1, numk
               nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
               work(k, l) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_SSX, st_ind(nodes, k), l))
            end do
         end do
         work = work / is_dtint
         field = 'susload_x_comp'
         ierr = nf90_put_var(imapfile, id_ssx(iid), work(1:numk, 1:lsedtot), [1, 1, itim], [ndxndxi, lsedtot, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
         !
         do l = 1, lsedtot
            do k = 1, numk
               nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
               work(k, l) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_SSY, st_ind(nodes, k), l))
            end do
         end do
         work = work / is_dtint
         field = 'susload_y_comp'
         ierr = nf90_put_var(imapfile, id_ssy(iid), work(1:numk, 1:lsedtot), [1, 1, itim], [ndxndxi, lsedtot, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
      end if

      !"soulsby"
      if ((trim(sedtrails_analysis) == 'soulsby' .or. trim(sedtrails_analysis) == 'all') .and. stm_included) then
         do k = 1, numk
            nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
            work(k, 1) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_HS, st_ind(nodes, k), 1))
         end do
         work = work / is_dtint
         field = 'waterdepth'
         ierr = nf90_put_var(imapfile, id_hs(iid), work(1:numk, 1), [1, itim], [ndxndxi, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
         !
         do k = 1, numk
            nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
            work(k, 1) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_TAUS, st_ind(nodes, k), 1))
         end do
         work = work / is_dtint
         field = 'mean_bss_magnitude'
         ierr = nf90_put_var(imapfile, id_taus(iid), work(1:numk, 1), [1, itim], [ndxndxi, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
         !
         do k = 1, numk
            nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
            work(k, 1) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_TAUSMAX, st_ind(nodes, k), 1))
         end do
         work = work / is_dtint
         field = 'max_bss_magnitude'
         ierr = nf90_put_var(imapfile, id_tausmax(iid), work(1:numk, 1), [1, itim], [ndxndxi, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
         !
         do l = 1, lsedtot
            do k = 1, numk
               nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
               work(k, l) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_SSC, st_ind(nodes, k), l))
            end do
         end do
         work = work / is_dtint
         field = 'suspended_sed_conc'
         ierr = nf90_put_var(imapfile, id_ssc(iid), work(1:numk, 1:lsedtot), [1, 1, itim], [ndxndxi, lsedtot, 1])
         if (ierr /= nf90_noerr) then
            return
         end if
         !
         if (jawave > NO_WAVES) then
            do k = 1, numk
               nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
               work(k, 1) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_UA, st_ind(nodes, k), 1))
            end do
            work = work / is_dtint
            field = 'wave_nonlin_vel_x_comp'
            ierr = nf90_put_var(imapfile, id_ua(iid), work(1:numk, 1), [1, itim], [ndxndxi, 1])
            if (ierr /= nf90_noerr) then
               return
            end if
            !
            do k = 1, numk
               nodes = pack([(ii, ii=1, size(st_ind(:, k)))], st_ind(:, k) > 0)
               work(k, 1) = sum(st_wf(nodes, k) * is_sumvalsnd(IDX_VA, st_ind(nodes, k), 1))
            end do
            work = work / is_dtint
            field = 'wave_nonlin_vel_y_comp'
            ierr = nf90_put_var(imapfile, id_va(iid), work(1:numk, 1), [1, itim], [ndxndxi, 1])
            if (ierr /= nf90_noerr) then
               return
            end if
         end if
      end if

      if (sedtrails_profiles_enabled()) then
         call write_sedtrails_profiles(imapfile, profile_ids, itim, ierr, field)
      end if
   end subroutine unc_write_sedtrails_filepointer

   !> Define layer-resolved fields on the existing time and point axes.
   subroutine define_sedtrails_profiles(ncid, nodes_id, time_dim, time_id, time_units, ids, ierr, field)
      use m_flow, only: kmx
      use m_missing, only: dmiss
      use m_sferic, only: jsferic

      integer, intent(in) :: ncid !< Output dataset.
      integer, intent(in) :: nodes_id !< Point dimension ID.
      integer, intent(in) :: time_dim !< Time dimension ID.
      integer, intent(in) :: time_id !< Time variable ID.
      character(len=*), intent(in) :: time_units !< Units of the time variable.
      type(t_profile_ids), intent(out) :: ids !< Profile variable IDs.
      integer, intent(out) :: ierr !< First failed NetCDF operation, or success.
      character(len=*), intent(out) :: field !< Context for an error.

      integer :: layer_dim, bounds_dim, component, dims(3)
      character(len=32) :: standard_names(3)
      character(len=*), parameter :: XY = 'net_xcc net_ycc'

      field = 'nLayers'
      ierr = nf90_def_dim(ncid, 'nLayers', kmx, layer_dim)
      if (ierr /= nf90_noerr) then
         return
      end if
      field = 'two'
      ierr = nf90_def_dim(ncid, 'two', 2, bounds_dim)
      if (ierr /= nf90_noerr) then
         return
      end if
      dims = [nodes_id, layer_dim, time_dim]

      field = 'layer'
      ierr = nf90_def_var(ncid, 'layer', nf90_int, [layer_dim], ids%layer)
      call attribute(ids%layer, 'units', '1')
      call attribute(ids%layer, 'long_name', 'native model layer index increasing upward')

      call define_field('layer_z', dims, ids%height, 'mean layer center elevation in the model vertical reference', 'm')
      call attribute(ids%height, 'positive', 'up')
      call attribute(ids%height, 'coordinates', XY//' layer')
      call attribute(ids%height, 'bounds', 'layer_z_bounds')
      call attribute(ids%height, 'cell_methods', 'time: mean')
      call define_field('layer_z_bounds', [bounds_dim, dims], ids%height_bounds, &
                        'mean lower and upper layer elevations', 'm')
      call define_field('layer_valid_time', dims, ids%duration, 'duration with a valid velocity and layer sample', 's')
      call attribute(ids%duration, 'coordinates', XY//' layer_z')
      call define_field('time_bounds', [bounds_dim, time_dim], ids%time_bounds, &
                        'actual averaging window of the solver samples', trim(time_units))
      call attribute(time_id, 'bounds', 'time_bounds')

      standard_names = [character(len=32) :: 'sea_water_x_velocity', 'sea_water_y_velocity', 'upward_sea_water_velocity']
      if (jsferic /= 0) then
         standard_names(1:2) = [character(len=32) :: 'eastward_sea_water_velocity', 'northward_sea_water_velocity']
      end if
      do component = 1, size(ids%velocity)
         call define_field(trim(VELOCITY_NAMES(component)), dims, ids%velocity(component), &
                           'mean layer-center velocity on SedTRAILS grid corner', 'm s-1')
         call attribute(ids%velocity(component), 'coordinates', XY//' layer_z layer')
         call attribute(ids%velocity(component), 'standard_name', trim(standard_names(component)))
         call attribute(ids%velocity(component), 'cell_methods', 'time: mean')
         call attribute(ids%velocity(component), 'ancillary_variables', 'layer_valid_time')
         if (component < 3) then
            call attribute(ids%velocity(component), 'comment', 'Horizontal velocity uses the model convention '// &
                           'of the depth-averaged SedTRAILS fields, including when waves are coupled.')
         end if
         if (ierr == nf90_noerr) then
            ierr = unc_add_gridmapping_att(ncid, [ids%velocity(component)], jsferic)
         end if
      end do

   contains

      !> Define a double field and retain the first failure for the caller.
      subroutine define_field(name, dimensions, var_id, description, units)
         character(len=*), intent(in) :: name !< NetCDF variable name.
         integer, intent(in) :: dimensions(:) !< Fortran dimension IDs.
         integer, intent(out) :: var_id !< NetCDF variable ID.
         character(len=*), intent(in) :: description !< Long name.
         character(len=*), intent(in) :: units !< Physical units.

         var_id = -1
         if (ierr /= nf90_noerr) then
            return
         end if
         field = name
         ierr = nf90_def_var(ncid, name, nf90_double, dimensions, var_id)
         call attribute(var_id, 'long_name', description)
         call attribute(var_id, 'units', units)
         if (ierr == nf90_noerr) then
            ierr = nf90_put_att(ncid, var_id, '_FillValue', dmiss)
         end if
      end subroutine define_field

      !> Set a text attribute if the definition is still successful.
      subroutine attribute(var_id, name, value)
         integer, intent(in) :: var_id !< NetCDF variable ID.
         character(len=*), intent(in) :: name !< Attribute name.
         character(len=*), intent(in) :: value !< Attribute value.

         if (ierr == nf90_noerr) then
            ierr = nf90_put_att(ncid, var_id, name, value)
         end if
      end subroutine attribute

   end subroutine define_sedtrails_profiles

   !> Write the velocity components, sampling geometry, and valid durations.
   subroutine write_sedtrails_profiles(ncid, ids, record, ierr, field)
      use m_sedtrails_stats, only: get_sedtrails_profiles
      use m_missing, only: dmiss

      integer, intent(in) :: ncid !< Output dataset.
      type(t_profile_ids), intent(in) :: ids !< Profile variable IDs.
      integer, intent(in) :: record !< Time record index.
      integer, intent(out) :: ierr !< First failed NetCDF operation, or success.
      character(len=*), intent(out) :: field !< Context for an error.

      real(kind=dp), allocatable :: values(:, :, :), duration(:, :), height(:, :)
      real(kind=dp) :: bounds(2)
      integer :: component, layer, layers, slab(3), extents(3)

      call get_sedtrails_profiles(values, duration, bounds)
      layers = size(duration, 2)
      slab = [1, 1, record]
      extents = [numk, layers, 1]
      if (record == 1) then
         field = 'layer'
         ierr = nf90_put_var(ncid, ids%layer, [(layer, layer=1, layers)])
         if (ierr /= nf90_noerr) then
            return
         end if
      end if
      do component = 1, size(ids%velocity)
         field = VELOCITY_NAMES(component)
         ierr = nf90_put_var(ncid, ids%velocity(component), values(component, :, :), start=slab, count=extents)
         if (ierr /= nf90_noerr) then
            return
         end if
      end do
      allocate (height(numk, layers), source=dmiss)
      where (duration > 0.0_dp)
         height = 0.5_dp * (values(4, :, :) + values(5, :, :))
      end where
      field = 'layer_z'
      ierr = nf90_put_var(ncid, ids%height, height, start=slab, count=extents)
      if (ierr /= nf90_noerr) then
         return
      end if
      field = 'layer_z_bounds'
      ierr = nf90_put_var(ncid, ids%height_bounds, values(4:5, :, :), start=[1, slab], count=[2, extents])
      if (ierr /= nf90_noerr) then
         return
      end if
      field = 'layer_valid_time'
      ierr = nf90_put_var(ncid, ids%duration, duration, start=slab, count=extents)
      if (ierr /= nf90_noerr) then
         return
      end if
      field = 'time_bounds'
      ierr = nf90_put_var(ncid, ids%time_bounds, bounds, start=[1, record], count=[2, 1])
   end subroutine write_sedtrails_profiles

   subroutine sedtrails_loadNetwork(filename, istat, jadoorladen)
      use m_missing
      use m_alloc
      use m_partitioninfo, only: jampi
      use m_sedtrails_data
      use m_qnerror

      implicit none

      character(*), intent(in) :: filename !< Name of file to be read (in current directory or with full path).
      integer, intent(out) :: istat !< Return status (0=success)
      integer, intent(in) :: jadoorladen

      integer :: k0, numkn, i
      logical :: jawel

      inquire (file=filename, exist=jawel)
      if (.not. jawel) then
         call mess(LEVEL_WARN, 'sedtrails_loadNetwork::Could not open '''//trim(filename)//'''')
         return
      end if

      if (jadoorladen == 0) then
         k0 = 0
      else
         k0 = numk
      end if

      ! New NetCDF net file
      call sedtrails_unc_read_net(filename, k0, numkn, istat)

      if (istat == 0) then
         numk = k0 + numkn

         ! Fill global domain mode numbers, before reducing the sedtrails grid to the submodel we're in
         if (jampi > 0) then
            call realloc(iglobal_s, numk, keepExisting=.false., fill=0)
            iglobal_s = [(i, i=1, numk)]
         end if
      else
         call qnerror('sedtrails_loadNetwork::Error while loading network from '''//trim(filename)//''', please inspect the preceding diagnostic output.', ' ', ' ')
      end if
   end subroutine sedtrails_loadNetwork

end module m_sedtrails_netcdf
