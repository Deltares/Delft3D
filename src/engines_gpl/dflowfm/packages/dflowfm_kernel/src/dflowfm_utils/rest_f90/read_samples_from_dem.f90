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

      subroutine read_samples_from_dem(filnam, jadoorladen)
         use dem
         use m_missing
         use m_samples
         use m_drawthis
         use m_readyy
         implicit none
         character(len=*), intent(in) :: filnam
         integer, intent(in) :: jadoorladen
         integer :: i, j, istep
         type(DEMInfo) :: dem_info
         integer, allocatable :: arr(:, :)
         double precision, allocatable :: xarr(:, :), yarr(:, :)
         character(len=10) :: TEX

         call savesam()
         if (jadoorladen == 0) then
            ns = 0
         end if

         call read_dem_file(trim(filnam), dem_info, xarr, yarr, arr)
         if (dem_info%rows <= 0 .or. dem_info%cols <= 0) then
            call message('No samples read from file ', filnam, ' ')
            return
         end if

         call increasesam(ns + dem_info%rows * dem_info%cols)

         write (TEX, '(I10)') dem_info%rows * dem_info%cols
         call READYY('Filtering '//trim(TEX)//' Samples Points', 0d0)

         istep = int(dem_info%rows / 100d0)
         do i = 1, dem_info%rows
            do j = 1, dem_info%cols
               if (arr(i, j) == NODATA) then
                  continue
               else
                  ns = ns + 1
                  xs(ns) = xarr(i, j)
                  ys(ns) = yarr(i, j)
                  zs(ns) = dble(arr(i, j))
               end if
            end do
            if (mod(i, istep) == 0) then
               call READYY(' ', min(1d0, dble(i) / dem_info%rows))
            end if
         end do
         deallocate (xarr, yarr, arr)
         call READYY(' ', -1d0)

         if (NS > 100000) NDRAW(32) = 7 ! Squares (faster than circles)
         if (NS > 500000) NDRAW(32) = 3 ! Small dots (fastest)

         write (TEX, '(I10)') NS
         call READYY('Sorting '//trim(TEX)//' Samples Points', 0d0)
         if (NS > 1) then
            call TIDYSAMPLES(XS, YS, ZS, IPSAM, NS, MXSAM, MYSAM)
            call get_samples_boundingbox()
            IPSTAT = IPSTAT_OK
         end if
         call READYY(' ', -1d0)
      end subroutine read_samples_from_dem
