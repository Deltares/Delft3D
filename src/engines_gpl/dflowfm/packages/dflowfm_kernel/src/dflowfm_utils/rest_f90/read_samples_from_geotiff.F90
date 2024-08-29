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

!> Read samples from a GeoTIFF file.
!! Samples are being stored in the global dataset of m_samples.
function read_samples_from_geotiff(filename) result(success)
   use MessageHandling
   use, intrinsic :: iso_c_binding
#ifdef HAVE_GDAL
   use fortranc
   use gdal
#endif
   use m_samples
   use m_samples_refine, only: iHesstat, iHesstat_DIRTY
   use string_module, only: strcmpi
   use m_drawthis
   use m_readyy
   implicit none

   character(len=*), intent(in) :: filename !< Path of the file to be read
   logical :: success !< Return value to describe success of the operations

#ifdef HAVE_GDAL
   ! local
   type(gdaldataseth) :: dataset ! Gdal dataset
   type(GDALRasterBandH) :: rasterband ! Selected raster band in GDAL dataset
   integer(kind=c_int) :: nx, ny, nz, offsetx, offsety ! Geometry of dataset
   real(kind=c_double), allocatable :: dbuffer(:, :, :) ! Buffer containing data of dataset (for 64-bit raster bands)
   real(kind=c_float), allocatable :: fbuffer(:, :, :) ! Buffer containing data of dataset (for 32-bit raster bands)
   integer(kind=c_int) :: rasterband_datatypesize

   real(kind=c_double) :: geotransform(6) ! Geo information of dataset
   double precision :: dxa, dya ! Pixel size
   double precision :: x0, y0 ! Origin
   integer :: i, j, istep ! Counters used for loops
   integer(kind=c_int) :: ierr ! Integer to store return values of C functions
   double precision :: eps ! Small value to be used with inequalities involving floating point numbers
   type(c_ptr) :: c_area_or_point
   logical :: is_area
   double precision :: pixeloffset

   ! Register all available gdal drivers
   call gdalallregister()

   call READYY('Reading GeoTIFF file', 0d0)
   ! Opening a GeoTIFF gdal dataset for reading
   dataset = gdalopen(trim(filename)//char(0), GA_ReadOnly)
   if (.not. gdalassociated(dataset)) then
      call mess(LEVEL_WARN, 'Error opening dataset on GeoTIFF file: '//trim(filename))
      goto 888
   end if

   ! Get dimensions of dataset
   nx = gdalgetrasterxsize(dataset)
   ny = gdalgetrasterysize(dataset)
   nz = gdalgetrastercount(dataset)

   ! Throw warning if TIFF with multiple layers is given
   if (nz /= 1) then
      call mess(LEVEL_WARN, 'GeoTIFF files with multiple layers are currently not supported: '//trim(filename))
      goto 888
   end if

   rasterband = gdalgetrasterband(dataset, 1)
   rasterband_datatypesize = gdalgetdatatypesize(gdalgetrasterdatatype(rasterband))

   ! Read tiff data into buffer and throw warning if something went wrong
   offsetx = 0
   offsety = 0
   if (rasterband_datatypesize == 32) then
      allocate (fbuffer(nx, ny, nz))
      ierr = gdaldatasetrasterio_f(dataset, GF_Read, offsetx, offsety, fbuffer)
   else if (rasterband_datatypesize == 64) then
      allocate (dbuffer(nx, ny, nz))
      ierr = gdaldatasetrasterio_f(dataset, GF_Read, offsetx, offsety, dbuffer)
   else
      write (msgbuf, '(a,a,i0,a)') 'Error reading '''//trim(filename)//''': GeoTIFF files with non-float raster bands are currently not supported.', &
         ' Detected data type size: ', rasterband_datatypesize, ' bits.'
      call warn_flush()
      goto 888
   end if

   if (ierr /= 0) then
      call mess(LEVEL_WARN, 'Could not read GeoTIFF data of '//trim(filename)//' into an array buffer')
      goto 888
   end if

   ! Get geotransform
   ierr = gdalgetgeotransform(dataset, geotransform)
   if (ierr /= 0) then
      call mess(LEVEL_WARN, 'Error getting the geotransform from dataset of file '//trim(filename))
      goto 888
   end if

   ! Get origin and pixel size from geotransform
   x0 = geotransform(1)
   y0 = geotransform(4)
   dxa = geotransform(2)
   dya = geotransform(6)

   ! Throw warning if rotated TIFF was given
   eps = 1d-6
   if (abs(geotransform(3)) > eps .or. abs(geotransform(5)) > eps) then
      call mess(LEVEL_WARN, 'Rotated GeoTIFF files are currently not supported: '//trim(filename))
      goto 888
   end if

   ! Detect whether data location should be interpreted as area (pixel) or point.
   ! In case of area: offset our internal sample coordinates xs,ys with +0.5*pixelsize,
   ! such that they represent the centre of the pixels.
   c_area_or_point = GDALGetMetadataItem(gdalmajorobjecth_new(dataset), 'AREA_OR_POINT'//c_null_char, c_null_char)
   if (.not. c_associated(c_area_or_point)) then
      ! Meta data AREA_OR_POINT not present, we assume Area (pixels).
      is_area = .true.
   else
      is_area = strcmpi(strtofchar(c_area_or_point, 5), 'Area', 4)
   end if

   if (is_area) then
      pixeloffset = .5d0
   else
      pixeloffset = 0d0
   end if

   call READYY('Reading GeoTIFF file', 0.5d0)

   ! Set global sample arrays xs, ys, zs of m_samples
   call increasesam(nx * ny)
   istep = max(int(nx / 100d0 + .5d0), 1)
   ns = 0
   do i = 1, nx
      if (mod(i, istep) == 0) then
         call readyy('Reading GeoTIFF file', min(1d0, .5d0 + .5d0 * dble(i) / nx))
      end if
      do j = ny, 1, -1
         ns = ns + 1
         xs(ns) = x0 + dxa * (i - 1 + pixeloffset) ! "-1" to convert to 0-based C, pixeloffset to get the middle of the pixel instead of its edge
         ys(ns) = y0 + dya * (j - 1 + pixeloffset)
         if (rasterband_datatypesize == 32) then
            zs(ns) = fbuffer(i, j, nz)
         else if (rasterband_datatypesize == 64) then
            zs(ns) = dbuffer(i, j, nz)
         end if
      end do
   end do
   ! mark samples as structured, and in supply block sizes
   mxsam = ny ! j is fastest running index
   mysam = nx
   ipstat = IPSTAT_NOTOK

   ! new sample set: no Hessians computed yet
   iHesstat = iHesstat_DIRTY

   if (allocated(fbuffer)) deallocate (fbuffer)
   if (allocated(dbuffer)) deallocate (dbuffer)
   call gdalclose(dataset)

   if (ns > 100000) ndraw(32) = 7 ! Squares (faster than circles)
   if (ns > 500000) ndraw(32) = 3 ! Small dots (fastest)

   ! No TIDYSAMPLES required: GeoTiff grid was already loaded in correctly sorted order.
   do i = 1, ns
      ipsam(i) = i
   end do
   call get_samples_boundingbox()
   ipstat = IPSTAT_OK

   call READYY('Reading GeoTIFF file', 1d0)
   success = .true.
   return

888 continue
   ! Some error occurred
   success = .false.
   if (gdalassociated(dataset)) then
      call gdalclose(dataset)
   end if

   if (allocated(fbuffer)) deallocate (fbuffer)
   if (allocated(dbuffer)) deallocate (dbuffer)

   return
#else
   call mess(LEVEL_WARN, 'GDAL is not available: cannot read GeoTIFF file '//trim(filename))
   success = .false.
#endif
end function read_samples_from_geotiff
