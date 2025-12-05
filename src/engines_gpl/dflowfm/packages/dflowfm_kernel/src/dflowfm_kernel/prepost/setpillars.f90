!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_setpillars

   implicit none

   private

   public :: setpillars

contains

   subroutine setpillars()
      use precision, only: dp
      use m_flowgeom, only: ndx, lnx, ba, wu, nd
      use fm_external_forcings_data, only: pillar, Cpil
      use m_vegetation, only: rnveg, diaveg, stemheight
      use gridoperations
      use m_flowparameters, only: japillar
      use m_crspath
      use m_crspath_on_flowgeom
      use unstruc_model, only: md_pillar_use_far_field_velocity

      integer :: i, j, k, L, Lf, La, m, n
      real(kind=dp) :: pi
      integer, dimension(:), allocatable :: npil
      real(kind=dp), dimension(:), allocatable :: cdeq !< combined pillar drag coefficient per cell or link
      real(kind=dp), dimension(:), allocatable :: Aeff !< effective area of cell (pillartype=1) of width of link (pillartype=3) available for flow (not occupied by pillars)
      real(kind=dp) :: velocity_squared_correction_per_width !< correction factor for velocity squared per unit width to be used in pillar drag calculation
      integer, dimension(:), allocatable :: linktype
      integer :: nPath
      type(tcrspath), dimension(:), allocatable :: Path
      integer, dimension(:), allocatable :: idum

      if (allocated(Cpil)) then
         deallocate (Cpil)
      end if
      if (japillar == 1) then
         allocate (Cpil(ndx))
      else if (japillar == 3) then
         allocate (Cpil(lnx))
      end if

      if (allocated(idum)) then
         deallocate (idum)
      end if
      allocate (idum(1))
      idum = 0

      pi = 4.0_dp * atan(1.0_dp)

      if (japillar == 2) then
         if (allocated(cdeq)) then
            deallocate (cdeq, npil)
         end if
         allocate (cdeq(ndx), npil(ndx))
         cdeq = 0.0_dp
         npil = 0
         do m = 1, size(pillar)
            do i = 1, pillar(m)%np
               if (pillar(m)%dia(i) == -999.0_dp .or. pillar(m)%cd(i) == -999.0_dp) then
                  cycle
               end if
               call incells(pillar(m)%xcor(i), pillar(m)%ycor(i), j)
               if (j == 0) then
                  cycle
               end if
               rnveg(j) = rnveg(j) + pillar(m)%dia(i)**2 * pi * 0.25_dp / ba(j)
               cdeq(j) = cdeq(j) + pillar(m)%cd(i) * pillar(m)%dia(i)
               npil(j) = npil(j) + 1
            end do
         end do
         do j = 1, ndx
            if (npil(j) == 0) then
               cycle
            end if
            diaveg(j) = diaveg(j) + cdeq(j) / npil(j)
            stemheight(j) = 1.0e30_dp
         end do
         deallocate (cdeq)
         deallocate (npil)

      elseif (japillar == 1) then ! Delft3D implimentation, but modified version on flow cells
         if (allocated(Aeff)) then
            deallocate (Aeff, cdeq)
         end if
         allocate (Aeff(ndx), cdeq(ndx))
         do j = 1, ndx
            Aeff(j) = ba(j)
         end do
         cdeq = 0.0_dp
         do m = 1, size(pillar)
            do i = 1, pillar(m)%np
               if (pillar(m)%dia(i) == -999.0_dp .or. pillar(m)%cd(i) == -999.0_dp) then
                  cycle
               end if
               call incells(pillar(m)%xcor(i), pillar(m)%ycor(i), j)
               if (j == 0) then
                  cycle
               end if
               cdeq(j) = cdeq(j) + pillar(m)%cd(i) * pillar(m)%dia(i)
               Aeff(j) = Aeff(j) - pillar(m)%dia(i)**2 * pi * 0.25_dp
            end do
         end do
         Cpil = 0.0_dp
         do j = 1, ndx
            if (cdeq(j) == 0) then
               cycle
            end if
            if (Aeff(j) <= 0.0_dp) then
               Cpil(j) = 1.0e30_dp
               cycle
            end if
            Cpil(j) = cdeq(j) * 0.25_dp / Aeff(j) * sqrt(ba(j) * pi)
         end do
         deallocate (Aeff)
         deallocate (cdeq)

      else if (japillar == 3) then ! Based on D3D approach on flow links
         if (allocated(Aeff)) then
            deallocate (Aeff, cdeq, linktype)
         end if
         allocate (Aeff(lnx), cdeq(lnx), linktype(lnx))
         linktype = 0
         Aeff = wu
         cdeq = 0.0_dp
         do m = 1, size(pillar)
            call pol_to_flowlinks(pillar(m)%xcor, pillar(m)%ycor, pillar(m)%xcor * 0.0_dp, pillar(m)%np, nPath, Path)
            do n = 1, nPath
               call crspath_on_flowgeom(Path(n), 1, 0, 1, idum, 0, 1)
               do L = 1, Path(n)%lnx
                  Lf = Path(n)%ln(L)
                  La = abs(Lf)
                  linktype(La) = 1
               end do
            end do
            do i = 1, pillar(m)%np
               if (pillar(m)%dia(i) == -999.0_dp .or. pillar(m)%cd(i) == -999.0_dp) then
                  cycle
               end if
               call incells(pillar(m)%xcor(i), pillar(m)%ycor(i), k)
               if (k == 0) then
                  cycle
               end if
               do L = 1, nd(k)%lnx
                  Lf = nd(k)%ln(L)
                  La = abs(Lf)
                  if (linktype(La) /= 1) then
                     cycle
                  end if
                  cdeq(La) = cdeq(La) + pillar(m)%cd(i) * pillar(m)%dia(i)
                  Aeff(La) = Aeff(La) - pillar(m)%dia(i)
               end do
            end do
         end do
         Cpil = 0.0_dp
         do L = 1, lnx
            if (cdeq(L) == 0) then
               cycle
            end if
            if (Aeff(L) <= 0.0_dp) then
               Cpil(L) = 1.0e30_dp
               cycle
            end if
            if (md_pillar_use_far_field_velocity == 1) then
               velocity_squared_correction_per_width = 1.0_dp / wu(L)
            else
               velocity_squared_correction_per_width = wu(L) / Aeff(L)**2
            end if
            Cpil(L) = cdeq(L) * 0.5_dp * velocity_squared_correction_per_width
         end do
         deallocate (Aeff)
         deallocate (cdeq)
         deallocate (linktype)
      end if

   end subroutine setpillars

end module m_setpillars
