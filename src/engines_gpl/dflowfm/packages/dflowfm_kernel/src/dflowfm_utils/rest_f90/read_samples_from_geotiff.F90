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

!> Load a GeoTIFF through the shared raster reader and convert it to global samples.
module m_read_samples_from_geotiff
   implicit none(type, external)
   private

   public :: read_samples_from_geotiff

   contains

   !> Legacy compatibility function, read raster data object and then convert to global samples.
   ! CAN BE REMOVED ONCE WE PHASE OUT OLD EXT
   function read_samples_from_geotiff(filename) result(success)
      use precision, only: dp
      use messagehandling, only: mess, level_warn
      use m_bilinear_interpolation, only: bilinear_data
      use timespace_parameters, only: GEOTIFF
      use m_samples, only: increasesam, ns, xs, ys, zs, mxsam, mysam, ipstat, ipsam, ipstat_ok
      use m_drawthis, only: ndraw
      use m_readyy, only: readyy
      use m_get_samples_boundingbox, only: get_samples_boundingbox
      use m_samples_refine, only: iHesstat, iHesstat_DIRTY

      character(len=*), intent(in) :: filename !< Path of the file to be read.
      logical :: success !< Whether the raster was read and converted successfully.

      type(bilinear_data) :: data
      integer :: status, nx, ny, i, j, istep

      success = .false.
      call readyy('Reading GeoTIFF file', 0.0_dp)
      data = bilinear_data(filename, GEOTIFF, status)
      if (status /= 0) then
         call mess(level_warn, 'Could not read GeoTIFF raster: '//trim(filename))
         call readyy('Reading GeoTIFF file', 1.0_dp)
         return
      end if

      nx = size(data%values, 1)
      ny = size(data%values, 2)
      call readyy('Reading GeoTIFF file', 0.5_dp)
      call increasesam(nx * ny)
      istep = max(nint(real(nx, kind=dp) / 100.0_dp), 1)
      ns = 0
      do i = 1, nx
         if (mod(i, istep) == 0) then
            call readyy('Reading GeoTIFF file', min(1.0_dp, 0.5_dp + 0.5_dp * real(i, kind=dp) / real(nx, kind=dp)))
         end if
         do j = ny, 1, -1
            ns = ns + 1
            xs(ns) = data%origin(1) + data%spacing(1) * real(i - 1, kind=dp)
            ys(ns) = data%origin(2) + data%spacing(2) * real(j - 1, kind=dp)
            zs(ns) = data%values(i, j)
         end do
      end do
      deallocate (data%values)

      ! Rows run fastest, as in the original GeoTIFF sample layout.
      mxsam = ny
      mysam = nx
      iHesstat = iHesstat_DIRTY

      if (ns > 100000) then
         ndraw(32) = 7
      end if
      if (ns > 500000) then
         ndraw(32) = 3
      end if

      do i = 1, ns
         ipsam(i) = i
      end do
      call get_samples_boundingbox()
      ipstat = ipstat_ok
      call readyy('Reading GeoTIFF file', 1.0_dp)
      success = .true.

   end function read_samples_from_geotiff

end module m_read_samples_from_geotiff
