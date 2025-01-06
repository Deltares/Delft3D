!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

! NOTES:
! - Observation points can be "moving" - these are excluded.
! - How about the global index to these items?
!

!> Manages the caching file - store and retrieve the grid-based information.
module unstruc_caching
   use precision, only: dp
   use m_observations_data, only: numobs, xobs, yobs, locTpObs, kobs, lobs
   use m_monitoring_crosssections, only: crs, tcrs, deallocCrossSections
   use md5_checksum, only: md5length, md5file
   use m_alloc, only: realloc
   use network_data, only: tface, netcell
   use string_module, only: get_version_major_minor_integer
   use m_crspath, only: tcrspath

   implicit none

   ! CachingFormatVersion = 1.01
   integer, parameter :: CacheFormatMajorVersion = 1
   integer, parameter :: CacheFormatMinorVersion = 1

   ! History CachingFormatVersion:

   ! 1.00 (Until Nov 2024): The cached items include observations, fixed weirs, cross sections and dry_points_and_area.
   ! 1.01 (Dec 2024)      : Thin dams are added to caching.

   logical, private :: cache_success

   character(len=20), dimension(5), private :: section = ['OBSERVATIONS        ', &
                                                          'FIXED WEIRS         ', &
                                                          'CROSS_SECTIONS      ', &
                                                          'DRY_POINTS_AND_AREAS', &
                                                          'THIN_DAMS']
   integer, parameter, private :: key_obs = 1
   integer, parameter, private :: key_fixed_weirs = 2
   integer, parameter, private :: key_cross_sections = 3
   integer, parameter, private :: key_dry_points_and_areas = 4
   integer, parameter, private :: key_thin_dams = 5

   real(kind=dp), dimension(:), allocatable, private :: cache_xobs
   real(kind=dp), dimension(:), allocatable, private :: cache_yobs
   real(kind=dp), dimension(:), allocatable, private :: cache_xpl_fixed
   real(kind=dp), dimension(:), allocatable, private :: cache_ypl_fixed
   real(kind=dp), dimension(:), allocatable, private :: cache_dsl_fixed
   integer, dimension(:), allocatable, private :: cache_locTpObs
   integer, dimension(:), allocatable, private :: cache_kobs
   integer, dimension(:), allocatable, private :: cache_lobs
   integer, dimension(:), allocatable, private :: cache_ilink_fixed
   integer, dimension(:), allocatable, private :: cache_ipol_fixed
   integer, dimension(:), allocatable, private :: cache_linklist
   integer, dimension(:), allocatable, private :: cache_ipol

   type(tcrs), dimension(:), allocatable :: cache_cross_sections

   integer, private :: cached_nump_dry
   integer, private :: cached_nump1d2d_dry
   integer, dimension(:, :), allocatable, private :: cached_lne_dry
   integer, dimension(:), allocatable, private :: cached_lnn_dry
   real(kind=dp), dimension(:), allocatable, private :: cached_xzw_dry
   real(kind=dp), dimension(:), allocatable, private :: cached_yzw_dry
   real(kind=dp), dimension(:), allocatable, private :: cached_bottom_area_dry
   real(kind=dp), dimension(:), allocatable, private :: cached_xz_dry
   real(kind=dp), dimension(:), allocatable, private :: cached_yz_dry
   type(tface), dimension(:), allocatable, private :: cached_netcell_dry

   type(tcrspath), dimension(:), allocatable, private :: cached_thin_dams

   character(len=23), parameter, private :: version_string_prefix = "D-Flow FM, cache file, "
   character(len=md5length), private :: md5current

contains
!> Sets ALL (scalar) variables in this module to their default values.
   subroutine default_caching()

      cache_success = .false.

      if (allocated(cache_xobs)) deallocate (cache_xobs)
      if (allocated(cache_yobs)) deallocate (cache_yobs)
      if (allocated(cache_xpl_fixed)) deallocate (cache_xpl_fixed)
      if (allocated(cache_ypl_fixed)) deallocate (cache_ypl_fixed)
      if (allocated(cache_dsl_fixed)) deallocate (cache_dsl_fixed)
      if (allocated(cache_locTpObs)) deallocate (cache_locTpObs)
      if (allocated(cache_kobs)) deallocate (cache_kobs)
      if (allocated(cache_lobs)) deallocate (cache_lobs)
      if (allocated(cache_ilink_fixed)) deallocate (cache_ilink_fixed)
      if (allocated(cache_ipol_fixed)) deallocate (cache_ipol_fixed)
      if (allocated(cache_linklist)) deallocate (cache_linklist)
      if (allocated(cache_ipol)) deallocate (cache_ipol)
      if (allocated(cached_lne_dry)) deallocate (cached_lne_dry)
      if (allocated(cached_lnn_dry)) deallocate (cached_lnn_dry)
      if (allocated(cached_xzw_dry)) deallocate (cached_xzw_dry)
      if (allocated(cached_yzw_dry)) deallocate (cached_yzw_dry)
      if (allocated(cached_bottom_area_dry)) deallocate (cached_bottom_area_dry)
      if (allocated(cached_xz_dry)) deallocate (cached_xz_dry)
      if (allocated(cached_yz_dry)) deallocate (cached_yz_dry)
      if (allocated(cached_netcell_dry)) deallocate (cached_netcell_dry)

      if (allocated(cache_cross_sections)) call deallocCrossSections(cache_cross_sections)

      if (allocated(cached_thin_dams)) deallocate (cached_thin_dams)

      md5current = ''

   end subroutine default_caching

!> Check that the caching file contained compatible information
   logical function cache_retrieved()
      cache_retrieved = cache_success
   end function cache_retrieved

!> Load the information from the caching file - if any.
   subroutine load_caching_file(base_name, net_file, use_caching)

      use MessageHandling, only: LEVEL_INFO, LEVEL_WARN, mess

      character(len=*), intent(in) :: base_name !< base_name to construct the name of the cache file (typically md_ident).
      character(len=*), intent(in) :: net_file !< Full name of the network file
      logical, intent(inout) :: use_caching !< Use the cache file if true. Might be reset to false if some errors forbid the use of caching.

      integer :: lun
      integer :: ierr
      integer :: number, number_links, number_nodes, number_netcells, number_thin_dams
      integer :: version_major, version_minor
      character(len=30) :: version_file
      character(len=20) :: key
      character(len=256) :: file_name
      character(len=256) :: msgbuf
      character(len=md5length) :: md5checksum
      logical :: success

      cache_success = .false.

      if (.not. use_caching) then
         call mess(LEVEL_INFO, 'Not using cache file.')
         return
      end if

      !
      ! Determine the MD5 checksum of the network file to use for creating new cache file
      ! and/or for checking compatibility of existing cache file:
      !
      call md5file(net_file, md5current, success)
      if (.not. success) then
         close (lun)
         call mess(LEVEL_WARN, 'Failed to read MD5 checksum of the network file '//trim(net_file) &
                   //', caching is not possible. Proceeding with normal initialization.')
         use_caching = .false.
         return
      end if

      file_name = trim(base_name)//'.cache'

      call mess(LEVEL_INFO, 'Reading cache file: '//trim(file_name))
      open (newunit=lun, file=trim(file_name), status="old", access="stream", iostat=ierr)

      !
      ! Apparently there is no caching file, so return without further processing
      !
      if (ierr /= 0) then
         call mess(LEVEL_INFO, 'No cache file available initially. Proceeding with normal initialization.')
         return
      end if

      !
      ! Load the version number and the MD5 checksum - useable at all?
      !
      read (lun, iostat=ierr) version_file, md5checksum

      if (ierr /= 0) then
         call mess(LEVEL_WARN, 'Failed to read cache file. Proceeding with normal initialization.')
         close (lun)
         return
      end if

      if (index(version_file, version_string_prefix) == 1) then
         version_file = adjustl(version_file(len_trim(version_string_prefix) + 1:))
         call get_version_major_minor_integer(version_file, version_major, version_minor, success)
         if (.not. success) then
            close (lun)
            call mess(LEVEL_WARN, 'Failed to read major and minor version from cache file. Proceeding with normal initialization.')
            return
         end if
         if (version_major /= CacheFormatMajorVersion .or. version_minor /= CacheFormatMinorVersion) then
            close (lun)
            write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Cache format mismatch. Cache file contains: v', version_major, &
               version_minor, '. Current format is: v', CacheFormatMajorVersion, CacheFormatMinorVersion, &
               ' (must be equal). Proceeding with normal initialization.'
            call mess(LEVEL_WARN, msgbuf)
            return
         end if
      else
         close (lun)
         call mess(LEVEL_WARN, 'Failed to read version information from cache file. Proceeding with normal initialization.')
         return
      end if

      !
      ! MD5 checksum of the network file must match the checksum in the cache file
      !
      if (md5checksum /= md5current) then
         close (lun)
         call mess(LEVEL_WARN, 'The MD5 checksum of the cache file does not match the network file. Proceeding with normal initialization.')
         return
      end if

      !
      ! Load the observation points:
      ! Copy the node numbers when successful
      !
      read (lun, iostat=ierr) key, number

      if (ierr /= 0 .or. key /= section(key_obs)) then
         call mess(LEVEL_WARN, 'Failed to load observation locations from cache file (none present). Proceeding with normal initialization.')
         close (lun)
         return
      end if

      call realloc(cache_xobs, number, keepExisting=.false.)
      call realloc(cache_yobs, number, keepExisting=.false.)
      call realloc(cache_locTpObs, number, keepExisting=.false.)
      call realloc(cache_kobs, number, keepExisting=.false.)
      call realloc(cache_lobs, number, keepExisting=.false.)

      if (number > 0) then
         read (lun, iostat=ierr) cache_xobs, cache_yobs, cache_locTpObs, cache_kobs, cache_lobs
      end if

      if (ierr /= 0) then
         call mess(LEVEL_WARN, 'Failed to load observation locations from cache file (invalid data). Proceeding with normal initialization.')
         close (lun)
         return
      end if

      !
      ! Load the information on the fixed weirs:
      ! Copy the link numbers when successful
      !
      read (lun, iostat=ierr) key, number, number_links

      if (ierr /= 0 .or. key /= section(key_fixed_weirs)) then
         call mess(LEVEL_WARN, 'Failed to load fixed weirs from cache file (none present). Proceeding with normal initialization.')
         close (lun)
         return
      end if

      call realloc(cache_xpl_fixed, number, keepExisting=.false.)
      call realloc(cache_ypl_fixed, number, keepExisting=.false.)
      call realloc(cache_ilink_fixed, number_links, keepExisting=.false.)
      call realloc(cache_ipol_fixed, number_links, keepExisting=.false.)
      call realloc(cache_dsl_fixed, number_links, keepExisting=.false.)

      if (number > 0) then
         read (lun, iostat=ierr) cache_xpl_fixed, cache_ypl_fixed, cache_ilink_fixed, cache_ipol_fixed, cache_dsl_fixed
      end if

      if (ierr /= 0) then
         call mess(LEVEL_WARN, 'Failed to load fixed weirs from cache file (invalid data). Proceeding with normal initialization.')
         close (lun)
         return
      end if

      !
      ! Load the information on the cross-sections:
      ! Copy all information when successful
      !
      read (lun, iostat=ierr) key, number, number_links

      if (ierr /= 0 .or. key /= section(key_cross_sections)) then
         call mess(LEVEL_WARN, 'Failed to load cross sections from cache file (none present). Proceeding with normal initialization.')
         close (lun)
         return
      end if

      allocate (cache_cross_sections(number))
      allocate (cache_linklist(number_links))
      allocate (cache_ipol(number_links))
      call load_cached_cross_sections(lun, cache_linklist, cache_ipol, cache_cross_sections, ierr)
      if (ierr /= 0) then
         call mess(LEVEL_WARN, 'Failed to load cross sections from cache file (invalid data). Proceeding with normal initialization.')
         close (lun)
         return
      end if

      !
      ! Load the information on grid with deleted dry points and areas:
      !
      read (lun, iostat=ierr) key, cached_nump_dry, cached_nump1d2d_dry, number_nodes, number_links, number_netcells
      if (ierr /= 0 .or. key /= section(key_dry_points_and_areas)) then
         call mess(LEVEL_WARN, 'Failed to load dry points and areas from cache file (none present). Proceeding with normal initialization.')
         close (lun)
         return
      end if
      allocate (cached_bottom_area_dry(number_nodes))
      allocate (cached_lne_dry(2, number_links))
      allocate (cached_lnn_dry(number_links))
      allocate (cached_xzw_dry(number_nodes))
      allocate (cached_yzw_dry(number_nodes))
      allocate (cached_xz_dry(number_nodes))
      allocate (cached_yz_dry(number_nodes))
      read (lun, iostat=ierr) cached_bottom_area_dry, cached_lne_dry, cached_lnn_dry, cached_xz_dry, cached_yz_dry, cached_xzw_dry, cached_yzw_dry
      if (ierr /= 0) then
         call mess(LEVEL_WARN, 'Failed to load dry points and areas from cache file (invalid data). Proceeding with normal initialization.')
         close (lun)
         return
      end if
      allocate (cached_netcell_dry(number_netcells))
      call load_netcell(lun, number_netcells, cached_netcell_dry, ierr)
      if (ierr /= 0) then
         call mess(LEVEL_WARN, 'Failed to load dry points and areas from cache file (invalid netcell). Proceeding with normal initialization.')
         close (lun)
         return
      end if

      !
      ! Load the information on thin dams:
      !
      read (lun, iostat=ierr) key, number_thin_dams
      if (ierr /= 0 .or. key /= section(key_thin_dams)) then
         call mess(LEVEL_WARN, 'Failed to load thin dams from cache file (none present). Proceeding with normal initialization.')
         close (lun)
         return
      end if
      if (number_thin_dams > 0) then
         allocate (cached_thin_dams(number_thin_dams))
         call load_thin_dams(lun, number_thin_dams, cached_thin_dams, ierr)
         if (ierr /= 0) then
            call mess(LEVEL_WARN, 'Failed to load thin dams from cache file (invalid data). Proceeding with normal initialization.')
            close (lun)
            return
         end if
      end if
      !
      ! All cached values were loaded, so all is well
      !
      close (lun)
      cache_success = .true.
      call mess(LEVEL_INFO, 'Succesfully read cache file: '//trim(file_name))

   end subroutine load_caching_file

!> Load cached thin dams from a caching file
   subroutine load_thin_dams(lun, number_thin_dams, thin_dams, ierr)
      integer, intent(in) :: lun !< LU-number of the caching file
      integer, intent(in) :: number_thin_dams !< Number of thin dams
      type(tcrspath), dimension(:), intent(out) :: thin_dams !< Thin dams path and set of crossed flow links
      integer, intent(out) :: ierr !< Error code

      integer :: i, number_flow_links, number_polyline_points

      do i = 1, number_thin_dams
         read (lun, iostat=ierr) number_flow_links, number_polyline_points
         if (ierr /= 0) then
            close (lun)
            exit
         end if
         read (lun, iostat=ierr) thin_dams(i)%np, thin_dams(i)%lnx
         if (ierr /= 0) then
            close (lun)
            exit
         end if
         allocate (thin_dams(i)%xp(number_polyline_points), thin_dams(i)%yp(number_polyline_points), &
                   thin_dams(i)%zp(number_polyline_points))
         read (lun, iostat=ierr) thin_dams(i)%xp, thin_dams(i)%yp, thin_dams(i)%zp
         if (ierr /= 0) then
            close (lun)
            exit
         end if
         if (thin_dams(i)%lnx > 0) then
            allocate (thin_dams(i)%ln(number_flow_links), thin_dams(i)%indexp(number_flow_links), &
                      thin_dams(i)%wfp(number_flow_links), thin_dams(i)%xk(2, number_flow_links), &
                      thin_dams(i)%yk(2, number_flow_links), thin_dams(i)%iperm(number_flow_links), &
                      thin_dams(i)%sp(number_flow_links), thin_dams(i)%wfk1k2(number_flow_links))
            read (lun, iostat=ierr) thin_dams(i)%ln, thin_dams(i)%indexp, thin_dams(i)%wfp, thin_dams(i)%xk, &
               thin_dams(i)%yk, thin_dams(i)%iperm, thin_dams(i)%sp, thin_dams(i)%wfk1k2
            if (ierr /= 0) then
               close (lun)
               exit
            end if
         end if
      end do
   end subroutine load_thin_dams

!> Load cached netcell from a caching file.
   subroutine load_netcell(lun, number_netcell, netcell, ierr)
      integer, intent(in) :: lun !< LU-number of the caching file
      integer, intent(in) :: number_netcell !< Number of netcells
      type(tface), dimension(:), intent(out) :: netcell !< (nump1d2d) 1D&2D net cells (nodes and links), a cell with net nodes as vertices.
      integer, intent(out) :: ierr !< Error code

      integer :: i, number_links, number_nodes

      do i = 1, number_netcell
         read (lun, iostat=ierr) netcell(i)%n
         if (ierr /= 0) then
            close (lun)
            exit
         end if
         if (netcell(i)%n > 0) then
            read (lun, iostat=ierr) number_nodes, number_links
            if (ierr /= 0) then
               close (lun)
               exit
            end if
            allocate (netcell(i)%nod(number_nodes), netcell(i)%lin(number_links))
            read (lun, iostat=ierr) netcell(i)%nod, netcell(i)%lin
            if (ierr /= 0) then
               close (lun)
               exit
            end if
         end if
      end do

   end subroutine load_netcell

!> Load cached cross sections from a caching file.
   subroutine load_cached_cross_sections(lun, linklist, ipol, sections, ierr)
      integer, intent(in) :: lun !< LU-number of the caching file
      integer, dimension(:), intent(out) :: linklist !< Cached list of crossed flow links
      integer, dimension(:), intent(out) :: ipol !< Cached polygon administration
      type(tcrs), dimension(:), intent(out) :: sections !< Array of cross-sections to be filled
      integer, intent(out) :: ierr !< Error code

      integer :: i, np, nlink

      ! If there is nothing to be cached, do not even try to read (D3DFMIQ-2193)
      if (size(linklist) == 0) then
         ierr = 0
         return
      end if

      read (lun, iostat=ierr) linklist
      if (ierr /= 0) then
         close (lun)
         return
      end if

      read (lun, iostat=ierr) ipol
      if (ierr /= 0) then
         close (lun)
         return
      end if

      do i = 1, size(sections)
         read (lun, iostat=ierr) np, nlink

         sections(i)%path%np = np
         sections(i)%path%lnx = nlink
         allocate (sections(i)%path%xp(np), sections(i)%path%yp(np), &
                   sections(i)%path%zp(np), sections(i)%path%indexp(nlink), &
                   sections(i)%path%xk(2, nlink), sections(i)%path%yk(2, nlink), &
                   sections(i)%path%wfp(nlink), &
                   sections(i)%path%iperm(nlink), sections(i)%path%wfk1k2(nlink), &
                   sections(i)%path%sp(nlink), sections(i)%path%ln(nlink))

         if (nlink > 0) then
            read (lun, iostat=ierr) sections(i)%path%xp, sections(i)%path%yp, &
               sections(i)%path%zp, sections(i)%path%indexp, &
               sections(i)%path%xk, sections(i)%path%yk, &
               sections(i)%path%wfp, &
               sections(i)%path%iperm, sections(i)%path%wfk1k2, &
               sections(i)%path%sp, sections(i)%path%ln
         else
            if (np > 0) then
               read (lun, iostat=ierr) sections(i)%path%xp, sections(i)%path%yp, &
                  sections(i)%path%zp
            end if
         end if
         if (ierr /= 0) then
            close (lun)
            exit
         end if
      end do
   end subroutine load_cached_cross_sections

!> Save the link list of crossed flow links for later storage in the caching file.
   subroutine save_link_list(length, linklist, ipol)
      integer, intent(in) :: length !< Length of the list of crossed flow links
      integer, dimension(:), intent(in) :: linklist !< List of crossed flow links to be saved
      integer, dimension(:), intent(in) :: ipol !< Polygon administration

      cache_linklist = linklist(1:length)
      cache_ipol = ipol(1:length)
   end subroutine save_link_list

!> Store the grid-based information in the caching file.
   subroutine store_caching_file(base_name, use_caching)
      use MessageHandling, only: LEVEL_INFO, mess
      character(len=*), intent(in) :: base_name !< base_name to construct the name of the caching file (typically md_ident).
      logical, intent(in) :: use_caching !< Write the caching file if true - in accordance with the user setting

      integer :: lun
      integer :: ierr
      character(len=256) :: file_name
      character(len=30) :: version_string

      !
      ! If no caching should be used, dispense with writing the caching file
      !
      if (.not. use_caching) then
         return
      end if

      !
      ! If cache files already exist and have been loaded successfully, no need to rewrite them
      !
      if (cache_success) then
         return
      end if

      file_name = trim(base_name)//'.cache'

      open (newunit=lun, file=trim(file_name), access="stream", status="old", action='read', iostat=ierr)

      if (ierr == 0) then
         close (lun, status="delete")
      end if
      open (newunit=lun, file=trim(file_name), access="stream")

      !
      ! Store version string and checksum (already determined at start-up)
      !
      write (version_string, '(a,i0,".",i2.2)') version_string_prefix, CacheFormatMajorVersion, CacheFormatMinorVersion
      call mess(LEVEL_INFO, 'Writing cache file: '//trim(file_name)//', version: '//trim(version_string))
      write (lun) version_string, md5current

      !
      ! Store the observation points
      !
      write (lun) section(key_obs), numobs
      if (numobs > 0) then
         write (lun) xobs(1:numobs), yobs(1:numobs), locTpObs(1:numobs), kobs(1:numobs), lobs(1:numobs)
      end if

      !
      ! Store the fixed weirs data
      !
      write (lun) section(key_fixed_weirs), size(cache_xpl_fixed), size(cache_ilink_fixed)

      if (size(cache_xpl_fixed) > 0) then
         write (lun) cache_xpl_fixed, cache_ypl_fixed, cache_ilink_fixed, cache_ipol_fixed, cache_dsl_fixed
      end if

      !
      ! Store the data for the cross-sections
      !
      if (.not. allocated(crs)) then
         allocate (crs(0))
      end if
      if (.not. allocated(cache_linklist)) then
         allocate (cache_linklist(0))
      end if
      if (.not. allocated(cache_ipol)) then
         allocate (cache_ipol(0))
      end if
      write (lun) section(key_cross_sections), size(crs)
      call store_cross_sections(lun, crs, cache_linklist, cache_ipol)

      !
      ! Store the data for the dry points and areas
      !
      write (lun) section(key_dry_points_and_areas), cached_nump_dry, cached_nump1d2d_dry, size(cached_bottom_area_dry), size(cached_lnn_dry), size(cached_netcell_dry, 1)
      write (lun) cached_bottom_area_dry, cached_lne_dry, cached_lnn_dry, cached_xz_dry, cached_yz_dry, cached_xzw_dry, cached_yzw_dry
      call store_netcell(lun, cached_netcell_dry)

      !
      ! Store data for thin dams
      !
      write (lun) section(key_thin_dams), size(cached_thin_dams, 1)
      call store_thin_dams(lun, cached_thin_dams)

      !
      ! We are done, so close the file
      !
      close (lun)

   end subroutine store_caching_file

!> Store thin dams to a caching file.
   subroutine store_thin_dams(lun, thin_dams)
      integer, intent(in) :: lun !< LU-number of the caching file
      type(tcrspath), dimension(:), intent(in) :: thin_dams !< Thin dams path and set of crossed flow links

      integer :: i, number_thin_dams, number_flow_links, number_polyline_points

      number_thin_dams = size(thin_dams, 1)
      if (number_thin_dams > 0) then
         do i = 1, number_thin_dams
            number_flow_links = size(thin_dams(i)%ln)
            number_polyline_points = size(thin_dams(i)%xp)
            write (lun) number_flow_links, number_polyline_points
            write (lun) thin_dams(i)%np, thin_dams(i)%lnx
            write (lun) thin_dams(i)%xp(1:number_polyline_points), thin_dams(i)%yp(1:number_polyline_points), &
               thin_dams(i)%zp(1:number_polyline_points)
            if (thin_dams(i)%lnx > 0) then
               write (lun) thin_dams(i)%ln(1:number_flow_links), thin_dams(i)%indexp(1:number_flow_links), &
                  thin_dams(i)%wfp(1:number_flow_links), thin_dams(i)%xk(1:2, 1:number_flow_links), &
                  thin_dams(i)%yk(1:2, 1:number_flow_links), thin_dams(i)%iperm(1:number_flow_links), &
                  thin_dams(i)%sp(1:number_flow_links), thin_dams(i)%wfk1k2(1:number_flow_links)
            end if
         end do
      end if

   end subroutine store_thin_dams

!> Store netcell to a caching file.
   subroutine store_netcell(lun, netcell)
      integer, intent(in) :: lun !< LU-number of the caching file
      type(tface), dimension(:), intent(in) :: netcell !< Nr. of 2d netcells.

      integer :: i, number_netcell, number_links, number_nodes
      number_netcell = size(netcell, 1)

      do i = 1, number_netcell
         write (lun) netcell(i)%n
         if (netcell(i)%n > 0) then
            number_nodes = size(netcell(i)%nod)
            number_links = size(netcell(i)%lin)
            write (lun) number_nodes, number_links
            write (lun) netcell(i)%nod(1:number_nodes), netcell(i)%lin(1:number_links)
         end if
      end do

   end subroutine store_netcell

!> Store cross sections to a caching file.
   subroutine store_cross_sections(lun, sections, linklist, ipol)
      integer, intent(in) :: lun !< LU-number of the caching file
      type(tcrs), dimension(:), intent(in) :: sections !< Array of cross-sections to be filled
      integer, dimension(:), intent(in) :: linklist !< List of crossed flow links
      integer, dimension(:), intent(in) :: ipol !< Polygon administration

      integer :: i, np, nlink

      write (lun) size(linklist)
      write (lun) linklist
      write (lun) ipol

      do i = 1, size(sections)
         np = sections(i)%path%np
         nlink = sections(i)%path%lnx
         write (lun) sections(i)%path%np, sections(i)%path%lnx

         if (nlink > 0) then
            write (lun) sections(i)%path%xp(1:np), sections(i)%path%yp(1:np), &
               sections(i)%path%zp(1:np), sections(i)%path%indexp(1:nlink), &
               sections(i)%path%xk(:, 1:nlink), sections(i)%path%yk(:, 1:nlink), &
               sections(i)%path%wfp(1:nlink), &
               sections(i)%path%iperm(1:nlink), sections(i)%path%wfk1k2(1:nlink), &
               sections(i)%path%sp(1:nlink), sections(i)%path%ln(1:nlink)
         else
            if (np > 0) then
               write (lun) sections(i)%path%xp(1:np), sections(i)%path%yp(1:np), &
                  sections(i)%path%zp(1:np)
            end if
         end if
      end do
   end subroutine store_cross_sections

!> Copy the cached network information for observation points.
   subroutine copy_cached_observations(success)
      logical, intent(out) :: success !< The cached information was compatible if true

      success = .false.
      if (cache_success) then
         !
         ! Check the number of observations
         !
         if (.not. allocated(cache_xobs)) then
            return
         else if (numobs /= size(cache_xobs)) then
            return
         end if
         !
         ! Check that the coordinates and the type are identical to the cached values
         !
         if (all(cache_xobs == xobs(1:numobs)) .and. all(cache_yobs == yobs(1:numobs)) .and. &
             all(cache_locTpObs == locTpObs(1:numobs))) then
            success = .true.
            kobs(1:numobs) = cache_kobs
            lobs(1:numobs) = cache_lobs
         end if
      end if
   end subroutine copy_cached_observations

!> Copy the cached network information for cross-sections
   subroutine copy_cached_cross_sections(linklist, ipol, success)
      integer, dimension(:), allocatable, intent(out) :: linklist !< Cached list of crossed flow links
      integer, dimension(:), allocatable, intent(out) :: ipol !< Polygon administration
      logical, intent(out) :: success !< The cached information was compatible if true

      integer :: i, np

      success = .false.

      if (cache_success) then
         !
         ! Check the number of observations
         !
         if (size(crs) /= size(cache_cross_sections)) then
            return
         end if
         !
         ! Check that the coordinates and the type are identical to the cached values
         ! Note: no check on zp, it seems filled with arbitrary data (at least in 2D?)
         !
         success = .true.
         do i = 1, size(cache_cross_sections)
            np = cache_cross_sections(i)%path%np
            if (np /= crs(i)%path%np) then
               success = .false.
               exit
            end if
            if (np == 0) cycle
            if (any(cache_cross_sections(i)%path%xp(1:np) /= crs(i)%path%xp(1:np)) .or. &
                any(cache_cross_sections(i)%path%yp(1:np) /= crs(i)%path%yp(1:np))) then
               success = .false.
               exit
            end if
         end do

         if (success) then
            linklist = cache_linklist
            ipol = cache_ipol

            do i = 1, size(cache_cross_sections)
               ! Rely on automatic (re)allocation)
               crs(i)%path%np = cache_cross_sections(i)%path%np
               crs(i)%path%lnx = cache_cross_sections(i)%path%lnx
               crs(i)%path%indexp = cache_cross_sections(i)%path%indexp
               crs(i)%path%xk = cache_cross_sections(i)%path%xk
               crs(i)%path%yk = cache_cross_sections(i)%path%yk
               crs(i)%path%wfp = cache_cross_sections(i)%path%wfp
               crs(i)%path%iperm = cache_cross_sections(i)%path%iperm
               crs(i)%path%wfk1k2 = cache_cross_sections(i)%path%wfk1k2
               crs(i)%path%sp = cache_cross_sections(i)%path%sp
               crs(i)%path%ln = cache_cross_sections(i)%path%ln
            end do
         end if
      end if
   end subroutine copy_cached_cross_sections

!> Copy the cached information on fixed weirs.
   subroutine copy_cached_fixed_weirs(npl, xpl, ypl, number_links, iLink, iPol, dSL, success)
      use precision, only: dp
      integer, intent(in) :: npl !< Number of points in the polylines making up the weirs
      real(kind=dp), dimension(:), intent(in) :: xpl !< X-coordinates of the polyline points for the weirs
      real(kind=dp), dimension(:), intent(in) :: ypl !< Y-coordinates of the polyline points for the weirs
      integer, intent(out) :: number_links !< Number of flow links that was cached
      real(kind=dp), dimension(:), intent(out) :: dSL !< Intersection distance of each flow link on polyline segments that were cached
      integer, dimension(:), intent(out) :: iLink !< Flow link numbers that were cached
      integer, dimension(:), intent(out) :: iPol !< Intersected polyline segment numbers that were cached
      logical, intent(out) :: success !< The cached information was compatible if true

      success = .false.
      if (cache_success) then
         !
         ! Check the number of coordinate pairs
         !
         if (.not. allocated(cache_xpl_fixed)) then
            return
         else if (npl /= size(cache_xpl_fixed)) then
            return
         end if
         !
         ! Check that the coordinates are identical to the cached values
         !
         if (all(cache_xpl_fixed == xpl(1:npl)) .and. all(cache_ypl_fixed == ypl(1:npl))) then
            success = .true.
            number_links = size(cache_iLink_fixed)
            iLink(1:number_links) = cache_iLink_fixed
            iPol(1:number_links) = cache_iPol_fixed
            dSL(1:number_links) = cache_dSL_fixed
         end if
      end if
   end subroutine copy_cached_fixed_weirs

!> cache_fixed_weirs:
!>     The arrays for fixed weirs are partly local - they do not reside in a
!>     module, so explicitly store them when we have the actual data
   subroutine cache_fixed_weirs(npl, xpl, ypl, number_links, iLink, iPol, dSL)
      use precision, only: dp
      integer, intent(in) :: npl !< Number of points in the polylines making up the weirs
      integer, intent(in) :: number_links !< Number of flow links that is to be cached
      real(kind=dp), dimension(:), intent(in) :: xpl !< X-coordinates of the polyline points for the weirs
      real(kind=dp), dimension(:), intent(in) :: ypl !< Y-coordinates of the polyline points for the weirs
      real(kind=dp), dimension(:), intent(in) :: dSL !< Intersection distance of each flow link on polyline segments that are to be cached
      integer, dimension(:), intent(in) :: iLink !< Flow link numbers to be cached
      integer, dimension(:), intent(in) :: iPol !< Intersected polyline segment number to be cached

      cache_xpl_fixed = xpl(1:npl)
      cache_ypl_fixed = ypl(1:npl)
      cache_iLink_fixed = iLink(1:number_links)
      cache_iPol_fixed = iPol(1:number_links)
      cache_dSL_fixed = dSL(1:number_links)
   end subroutine cache_fixed_weirs

!> Copy grid information, where dry points and areas have been deleted, from cache file:
   subroutine copy_cached_netgeom_without_dry_points_and_areas(nump, nump1d2d, lne, lnn, bottom_area, xz, yz, xzw, yzw, netcell, success)
      use precision, only: dp
      integer, intent(out) :: nump !< Nr. of 2d netcells.
      integer, intent(out) :: nump1d2d !< nr. of 1D and 2D netcells (2D netcells come first)
      integer, dimension(:, :), intent(out) :: lne !< (2,numl) Edge administration 1=nd1 , 2=nd2, rythm of kn flow nodes between/next to which this net link lies.
      integer, dimension(:), intent(inout) :: lnn !< (numl) Nr. of cells in which link participates (ubound for non-dummy values in lne(:,L))
      real(kind=dp), dimension(:), intent(inout) :: bottom_area !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
      real(kind=dp), dimension(:), intent(out) :: xz !< [m/degrees_east] waterlevel point / cell centre, x-coordinate (m) {"location": "face", "shape": ["ndx"]}
      real(kind=dp), dimension(:), intent(out) :: yz !< [m/degrees_north] waterlevel point / cell centre, y-coordinate (m) {"location": "face", "shape": ["ndx"]}
      real(kind=dp), dimension(:), intent(out) :: xzw !< [m] x-coordinate, centre of gravity {"shape": ["nump"]}
      real(kind=dp), dimension(:), intent(out) :: yzw !< [m] y-coordinate, centre of gravity {"shape": ["nump"]}
      type(tface), dimension(:), intent(inout) :: netcell !< (nump1d2d) 1D&2D net cells (nodes and links)
      logical, intent(out) :: success !< The cached information was compatible if true

      integer number_nodes, number_links, number_netcells

      number_nodes = size(bottom_area)
      number_links = size(lnn)
      number_netcells = size(netcell)

      success = .false.
      if (cache_success) then
         if (.not. allocated(cached_netcell_dry)) then
            return
         end if
         if (size(bottom_area) == size(cached_bottom_area_dry)) then
            success = .true.
            nump = cached_nump_dry
            nump1d2d = cached_nump1d2d_dry
            lne = cached_lne_dry(1:2, 1:number_links)
            lnn = cached_lnn_dry(1:number_links)
            xzw = cached_xzw_dry(1:number_nodes)
            yzw = cached_yzw_dry(1:number_nodes)
            bottom_area = cached_bottom_area_dry(1:number_nodes)
            xz = cached_xz_dry(1:number_nodes)
            yz = cached_yz_dry(1:number_nodes)
            netcell = cached_netcell_dry(1:number_netcells)
         end if
      end if
   end subroutine copy_cached_netgeom_without_dry_points_and_areas

!> Copy grid information, links that have been inactivated due to thin dams, from cache file:
   subroutine copy_cached_thin_dams(thin_dams, success)
      type(tcrspath), dimension(:), intent(inout) :: thin_dams !< Thin dams path and set of crossed flow links
      logical, intent(out) :: success !< The cached information was compatible if true

      success = .false.
      if (cache_success) then
         if (.not. allocated(cached_thin_dams)) then
            return
         end if
         success = .true.
         thin_dams = cached_thin_dams
      end if
   end subroutine copy_cached_thin_dams

!> Cache grid information, where dry points and areas have been deleted:
   subroutine cache_netgeom_without_dry_points_and_areas(nump, nump1d2d, lne, lnn, bottom_area, xz, yz, xzw, yzw, netcell)
      use precision, only: dp
      integer, intent(in) :: nump !< Nr. of 2d netcells.
      integer, intent(in) :: nump1d2d !< nr. of 1D and 2D netcells (2D netcells come first)
      integer, dimension(:, :), intent(in) :: lne !< (2,numl) Edge administration 1=nd1 , 2=nd2, rythm of kn flow nodes between/next to which this net link lies.
      integer, dimension(:), intent(in) :: lnn !< (numl) Nr. of cells in which link participates (ubound for non-dummy values in lne(:,L))
      real(kind=dp), dimension(:), intent(in) :: bottom_area !< [m2] bottom area, if < 0 use table in node type {"location": "face", "shape": ["ndx"]}
      real(kind=dp), dimension(:), intent(in) :: xz !< [m/degrees_east] waterlevel point / cell centre, x-coordinate (m) {"location": "face", "shape": ["ndx"]}
      real(kind=dp), dimension(:), intent(in) :: yz !< [m/degrees_north] waterlevel point / cell centre, y-coordinate (m) {"location": "face", "shape": ["ndx"]}
      real(kind=dp), dimension(:), intent(in) :: xzw !< [m] x-coordinate, centre of gravity {"shape": ["nump"]}
      real(kind=dp), dimension(:), intent(in) :: yzw !< [m] y-coordinate, centre of gravity {"shape": ["nump"]}
      type(tface), dimension(:), intent(in) :: netcell !< (nump1d2d) 1D&2D net cells (nodes and links)
      integer number_nodes
      integer number_links
      integer number_netcells

      cached_nump_dry = nump
      cached_nump1d2d_dry = nump1d2d
      number_nodes = size(bottom_area)
      number_links = size(lnn)
      number_netcells = size(netcell)
      cached_lne_dry = lne(1:2, 1:number_links)
      cached_lnn_dry = lnn(1:number_links)
      cached_xzw_dry = xzw(1:number_nodes)
      cached_yzw_dry = yzw(1:number_nodes)
      cached_bottom_area_dry = bottom_area(1:number_nodes)
      cached_xz_dry = xz(1:number_nodes)
      cached_yz_dry = yz(1:number_nodes)
      cached_netcell_dry = netcell(1:number_netcells)

   end subroutine cache_netgeom_without_dry_points_and_areas

!> Cache thin dams:
   subroutine cache_thin_dams(thin_dams)
      type(tcrspath), dimension(:), intent(in) :: thin_dams !< Thin dams path and set of crossed flow links

      cached_thin_dams = thin_dams

   end subroutine cache_thin_dams

end module unstruc_caching
