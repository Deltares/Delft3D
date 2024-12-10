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

!> This module reads files in USGS Digital Elevation Model (DEM) format.
!!
!! This is a Fortran version of dem2geoeg.c originally by Dr Martin Reddy
!! http://www.ai.sri.com/~reddy/geovrml/dem2geoeg/
!! Plus some bugfixes handling the fixed 1024-byte size of logical records.
!!
!! Validated for projected coordinates without rotation.
module dem
   use precision, only: dp
   implicit none

   private
   public :: DEMInfo, NODATA, read_dem_file

!> Compound type for all relevant data in the DEM header.
   type DEMInfo
      character(len=145) :: mapLabel !< DEM quadrangle name
      integer :: DEMlevel !< 1 = DEM-1, 2 = DEM-2, 3 = DEM-3
      integer :: elevationPattern !< 1 = regular, 2 = future use
      integer :: groundSystem !< 0 = geographic, 1 = UTM, 2 = state plane
      integer :: groundZone !< UTM zone or stat plane code
      real(kind=dp) :: projectParams(15) !< parameters for various projections
      integer :: planeUnitOfMeasure !< 0 = radians, 1 = ft, 2 = m, 3 = arcsec
      integer :: elevUnitOfMeasure !< 1 = feet, 2 = meters
      integer :: polygonSizes !< sides of DEM polygon
      real(kind=dp) :: groundCoords(2, 4) !< coords of all 4 corners, SW first
      real(kind=dp) :: elevBounds(2) !< min/max elevations for the DEM
      real(kind=dp) :: localRotation !< counterclockwise angle in radians
      integer :: accuracyCode !< 0 = unknown, 1 = info in record C
      real(kind=dp) :: spatialResolution(3) !< normally 30,30,1 for 7.5-min DEMs
      integer :: profileDimension(2) !< rows and columns of DEM profiles

      ! The next fields are computed from the previous values

      real(kind=dp) :: eastMost, westMost !< furthest east/west easting value
      real(kind=dp) :: southMost, northMost !< furthest north/south northing value
      integer :: eastMostSample !< furthest east sample point
      integer :: westMostSample !< furthest west sample point
      integer :: southMostSample !< furthest south sample point
      integer :: northMostSample !< furthest north sample point
      integer :: rows, cols !< regular elevation grid dimensions
      character(len=41) :: shortName !< short version of map name
      integer :: maxElevation !< largest integer read
      integer :: minElevation !< smallest integer read

      ! Finally, we have a number of options for the program

      integer :: stitch_edges !< do stitching of edges were not defined?
      ! AvD: more settings for original program were removed here.

   end type DEMInfo

   integer, parameter :: NODATA = -32767
   integer, parameter :: SW = 1
   integer, parameter :: NW = 2
   integer, parameter :: NE = 3
   integer, parameter :: SE = 4

contains
!
!------------------------------------------------------------------------------

!> Reads an entire DEM file with elevation raster data into three arrays.
!!
!! \param filename Name of the DEM file to be read.
!! \param dem_info struct in which DEM meta-info will be stored.
!! \param xarr 2D array with raster x coordinates for all elevation samples.
!! \param yarr 2D array with raster y coordinates for all elevation samples.
!! \param  arr 2D array with raster elevation data (in dem_info\%elevUnitOfMeasure units).
   subroutine read_dem_file(filename, dem_info, xarr, yarr, arr)
      use precision, only: dp
      use unstruc_messages
      character(len=*), intent(in) :: filename
      type(DEMInfo), intent(inout) :: dem_info
      real(kind=dp), allocatable, intent(out) :: xarr(:, :), yarr(:, :)
      integer, allocatable, intent(out) :: arr(:, :)
!
! Local variables
!
      integer :: fp, istat
!
!! executable statements -------------------------------------------------------
!
      open (newunit=fp, file=trim(filename), action='read', iostat=istat)
      if (istat /= 0) then
         call message('DEM file could not be opened: ', trim(filename), ' ')
      end if

      call read_dem_header(fp, dem_info)

      if (dem_info%rows <= 0 .or. dem_info%cols <= 0) then
         write (msgbuf, '(a,i10,a,i10)') 'DEM file contains no elevation data: rows=', dem_info%rows, ', cols=', dem_info%cols
         call msg_flush()
         return
      end if

      if (allocated(arr)) deallocate (arr, xarr, yarr)
      allocate (arr(dem_info%rows, dem_info%cols), &
                xarr(dem_info%rows, dem_info%cols), &
                yarr(dem_info%rows, dem_info%cols))
      call read_dem_data(fp, dem_info, arr)

      call get_dem_grid(dem_info, xarr, yarr)

      close (fp)
   end subroutine read_dem_file
!
!------------------------------------------------------------------------------

!> Builds complete grid of 2D raster coordinates based on DEMInfo.
!!
!! Output arrays should be preallocated to dem_info%rows x dem_info%cols.
!!
!! \param dem_info struct containing the DEM meta-info.
!! \param xarr 2D array for raster x coordinates for all elevation samples (including missing).
!! \param yarr 2D array for raster y coordinates for all elevation samples (including missing).
   subroutine get_dem_grid(dem_info, xarr, yarr)
      use precision, only: dp
      type(DEMInfo), intent(in) :: dem_info
      real(kind=dp), intent(out) :: xarr(:, :), yarr(:, :)
!
! Local variables
!
      integer :: i, j
!
!! executable statements -------------------------------------------------------
!
      do j = 1, dem_info%cols
         do i = 1, dem_info%rows
            xarr(i, j) = (dem_info%eastMostSample + j - 1) * dem_info%spatialResolution(1)
            yarr(i, j) = (dem_info%southMostSample + i - 1) * dem_info%spatialResolution(2)
         end do
      end do
   end subroutine get_dem_grid
!
!------------------------------------------------------------------------------

!> Reads DEM meta-info from open file pointer.
!!
!! \param fp Opened DEM file pointer, pointing to beginning (start of A-type record).
!! \param dem_info struct in which the DEM meta-info will be stored.
   subroutine read_dem_header(fp, dem_info)
      integer, intent(in) :: fp
      type(DEMInfo), intent(inout) :: dem_info
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
      call message('Reading the DEM header...', ' ', ' ')

      ! Read the first five data elements in the DEM file

      read (fp, '(a144,4i6)', advance='no') &
         dem_info%mapLabel, & !< quadrangle name field
         dem_info%DEMlevel, & !< DEM level code
         dem_info%elevationPattern, & !< pattern code
         dem_info%groundSystem, & !< planimetric code
         dem_info%groundZone !< UTM zone

      ! there is no magic number to identify DEM files, but we can
      ! at least check to see if we have valid values for the first
      ! few fields to check that we have been given a USGS DEM file

      if (dem_info%DEMlevel <= 0 .or. dem_info%DEMlevel > 3) then
         call message('input file is not a valid USGS DEM!', ' ', ' ')
         return
      end if

      ! read in the projection parameters. These can be ignored for
      ! geographic, UTM, and state plane DEMs, i.e. most of the time

      read (fp, '(15D24.15)', advance='no') dem_info%projectParams

      ! read the next three data elements from the DEM file

      read (fp, '(3i6)', advance='no') &
         dem_info%planeUnitOfMeasure, & !< units for ground coords
         dem_info%elevUnitOfMeasure, & !< units for elevation coords
         dem_info%polygonSizes !< DEM polygon sides (4)

      ! next we have the 4 x 2 array of ground coordinates for the corners

      read (fp, '(4(2D24.15))', advance='no') dem_info%groundCoords

      ! and then another three data elements: the min and max
      ! elevation values and the degree of counterclockwise rotation

      read (fp, '(2D24.15)', advance='no') dem_info%elevBounds !< min and max elev
      read (fp, '(D24.15)', advance='no') dem_info%localRotation !< ccw rot.

      ! next we have the final data elements that we are concerned with

      read (fp, '(i6,3E12.5,2i6)', advance='no') &
         dem_info%accuracyCode, & !< non or unknown accuracy
         dem_info%spatialResolution, & !< x, y, z spatial resolution
         dem_info%profileDimension !< number of rows and columns

      ! old DEM format stops here - just read over the last 160 bytes
      read (fp, '(160X)', advance='no')

      ! we are now done reading the DEM header, however, now we will
      ! calculate some values from these for later convenience.

      ! work out the extremities of the DEM and then use these to
      ! calculate the number of rows and columns we have. N.B.
      ! DEMs have their corners rectified against lat/long, so
      ! this creates a rotated and skewed bounding box in UTM.

      dem_info%eastMost = max(dem_info%groundCoords(1, NE), &
                              dem_info%groundCoords(1, SE))
      dem_info%westMost = min(dem_info%groundCoords(1, NW), &
                              dem_info%groundCoords(1, SW))
      dem_info%northMost = max(dem_info%groundCoords(2, NE), &
                               dem_info%groundCoords(2, NW))
      dem_info%southMost = min(dem_info%groundCoords(2, SW), &
                               dem_info%groundCoords(2, SE))

      dem_info%eastMostSample = &
         int(dem_info%eastMost / dem_info%spatialResolution(1)) &
         * int(dem_info%spatialResolution(1))
      dem_info%westMostSample = &
         int(dem_info%westMost / dem_info%spatialResolution(1)) &
         * int(dem_info%spatialResolution(1))
      dem_info%northMostSample = &
         int(dem_info%northMost / dem_info%spatialResolution(2)) &
         * int(dem_info%spatialResolution(2))
      dem_info%southMostSample = &
         int(dem_info%southMost / dem_info%spatialResolution(2)) &
         * int(dem_info%spatialResolution(2))

      ! now that we now the furthest points, we can easily work out
      ! how many samples we need for each row and column (N.B. DEMs
      ! are not regular m x n grids of values due to the fact that
      ! the boundaries are aligned in lat/long but the data is
      ! specified in UTM coordinates.

      dem_info%cols = (dem_info%eastMostSample - dem_info%westMostSample) &
                      / int(dem_info%spatialResolution(1)) + 1
      dem_info%rows = (dem_info%northMostSample - dem_info%southMostSample) &
                      / int(dem_info%spatialResolution(2)) + 1

      ! create a short version of the quadrangle name by just taking
      ! the first 40 characters - this lobs off the other 100 or so
      ! bytes for things like process code, sectional indicator, etc
!
!  strncpy( dem_info->shortName, dem_info->mapLabel, 40 )
!  dem_info->shortName(40) = '\0'
!
!  for ( i = 39 i > 0 i-- )
!    if ( isspace( dem_info->shortName(i) ) )
!      dem_info->shortName(i) = '\0'
!    else
!      break
   end subroutine read_dem_header
!
!------------------------------------------------------------------------------

!> Read actual DEM data (type B blocks) from file pointer into 2D array.
!!
!! \param fp Open DEM file to read from, file pointer should be at start
!!           of first B block.
!! \param dem_info DEMInfo as produced by read_dem_header.
!! \param arr(:,:) 2D array to store elevation raster data in.
   subroutine read_dem_data(fp, dem_info, arr)
      use precision, only: dp
      use unstruc_messages
      use m_readyy
      integer, intent(in) :: fp
      type(DEMInfo), intent(inout) :: dem_info
      integer, intent(out) :: arr(:, :)
!
! Local variables
!
      integer :: c, r, num
      integer :: tempInt, lastProfile
      integer :: firstRow, lastRow
      integer :: yspacing
      integer :: profileID(2), profileSize(2)
      real(kind=dp) :: planCoords(2), localElevation, elevExtremea(2)
      character(len=7) :: fmt
!
!! executable statements -------------------------------------------------------
!
      yspacing = int(dem_info%spatialResolution(2))
      lastProfile = 0

      ! sample the DEM values into our regular grid

      call READYY('Reading DEM data', 0d0)
      do c = 1, dem_info%cols

         ! Read the Logical Record Type B at the start of the column
         ! to find out about the rest of the data in this profile

         read (fp, '(I6,I6,I6,I6)', advance='no') &
            profileID, & !< row and column identification number
            profileSize !< number of elevations in row and column (always 1)

         read (fp, '(5D24.25)', advance='no') &
            planCoords, & !< 1st ground coord
            localElevation, & !< local datum elev
            elevExtremea !< profile min and max elev

         ! check for end of processing
         if (profileID(2) - 1 /= lastProfile) then
            return
         else
            lastProfile = profileID(2)
         end if

         ! display a status message to say how far we've got

         write (msgbuf, '(a,i6,a,i6,a)') 'Reading column ', profileID(2), ' (', profileSize(1), ' rows)...'
         call msg_flush()
         if (mod(c, 20) == 0) then
            call READYY(' ', min(1d0, dble(c) / dem_info%cols))
         end if

         ! read in all the data for this column

         firstRow = int((planCoords(2) - dem_info%southMostSample) / yspacing) + 1
         lastRow = firstRow + profileSize(1) - 1

         do r = 1, dem_info%rows
            if (r < firstRow .or. r > lastRow) then
!             if ( dem_info%stitch_edges == 1) then
!                 arr(r,c) = NODATA
!             end if
               continue
            end if
            read (fp, '(I6)', advance='no') tempInt

            ! Handle Z-Resolution
            if (tempInt /= 0 .and. dem_info%spatialResolution(3) /= 1d0) then
               tempInt = int(tempInt * dem_info%spatialResolution(3))
               !status( "zRez %f before = %d after = %d", dem_info->spatialResolution(2), t, tempInt)
            end if
            dem_info%maxElevation = max(dem_info%maxElevation, tempInt)
            if (dem_info%minElevation == NODATA) then
               dem_info%minElevation = tempInt
            else
               dem_info%minElevation = min(dem_info%minElevation, tempInt)
            end if

            if (dem_info%elevUnitOfMeasure == 1) then ! convert ft to meters
               tempInt = int(real(tempInt) * 0.3048)
            end if

            if (r <= dem_info%rows) then
               arr(r, c) = tempInt
            end if
            planCoords(2) = planCoords(2) + yspacing

            ! Logical records have size 1024. Bytes 1021--1024 are padded with blanks.
            ! This means: skip 4 chars before reading row 147, 147+170, 147+2*170, etc.
            if (r == 146 .or. (r > 146 .and. mod(r - 146, 170) == 0)) then
               read (fp, '(4X)', advance='no')
            end if
         end do
         r = r - 1
         if (r < 147) then
            num = 1024 - 144 - r * 6
         else
            num = 1024 - mod(r - 146, 170) * 6
         end if

         ! Build the format string '(nX)' to read up to end of current 1024-byte logical record.
         fmt = ' '
         if (num < 10) then
            write (fmt, '(A1,I1,A2)') '(', num, 'X)'
         else if (num < 100) then
            write (fmt, '(A1,I2,A2)') '(', num, 'X)'
         else if (num < 1000) then
            write (fmt, '(A1,I3,A2)') '(', num, 'X)'
         else
            write (fmt, '(A1,I4,A2)') '(', num, 'X)'
         end if

         read (fp, trim(fmt), advance='no')

      end do ! c=1,dem_info%cols

      call READYY(' ', -1d0)

   end subroutine read_dem_data
!
!------------------------------------------------------------------------------
end module dem
