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

!> add internal tides friction forces to adve
module m_add_internaltidesfrictionforces
   use m_comp_gradc, only: comp_gradC

   implicit none

   private

   public :: add_InternalTidesFrictionForces

contains

   subroutine add_InternalTidesFrictionForces()
      use precision, only: dp
      use m_flowgeom
      use m_flow
      use m_flowtimes
      use unstruc_messages
      use m_partitioninfo
      use m_nod2linx, only: nod2linx
      use m_nod2liny, only: nod2liny
      implicit none

      real(kind=dp) :: GradHinUc, dum, Lambda, dfac
      real(kind=dp) :: dumx1, dumy1, dumx2, dumy2
      real(kind=dp) :: diss

      integer :: k, k1, k2, L
      integer :: ierror

!     compute water depth gradient, based on cell-centered date
      hs = s1 - bl
      call comp_gradC(hs, workx, worky)

      !call realloc(plotlin, Ndx, keepExisting=.false.)
      !plotlin = worky

!     compute cell-centered forces
      ierror = 0
      DissInternalTides = 0d0
      do k = 1, Ndx
         dum = sqrt(workx(k)**2 + worky(k)**2)
         GradHinUc = workx(k) * ucx(k) + worky(k) * ucy(k)
         workx(k) = -FrcInternalTides2D(k) * GradHinUc * workx(k) ! (m/s)*(m/s)*( )
         worky(k) = -FrcInternalTides2D(k) * GradHinUc * worky(k)

         if (hs(k) > epshs) then

            if (ITcap > 0d0) then
!              limit with ITcap
               diss = -rho(k) * (workx(k) * ucx(k) + worky(k) * ucy(k)) ! (kg/m3)*(m3/s3) = kg/s3

               if (diss > ITcap) then ! (W/m2) = (kg/s3)
                  dfac = ITcap / diss ! ( )

                  workx(k) = dfac * workx(k) ! still (m2/s2)
                  worky(k) = dfac * worky(k)
               end if
            end if

!           check time step
!           estimate eigenvalue
            Lambda = FrcInternalTides2D(k) * dum**2 / hs(k) ! (m/s)*( )/ (m) = (1/s)
            if (Lambda * dts > 1d0) then
               dfac = 1d0 / (Lambda * dts)
               !              write(str, "('k = ', I8, ': gamma ||grad H||^2 / H = ', E15.5, ' > 1/Delta t =', E15.5, ', H=', E15.5, ', ||grad H||=', E15.5, ', gamma=', E15.5, ', reduce factor=', E15.5)") k, Lambda, 1d0/dts, hs(k), dum, FrcInternalTides2D(k), dfac
               !              call mess(LEVEL_WARN, trim(str))
!               ierror = 1

               workx(k) = dfac * workx(k) ! still (m2/s2)
               worky(k) = dfac * worky(k)
            end if

            DissInternalTidesPerArea(k) = -rho(k) * (workx(k) * ucx(k) + worky(k) * ucy(k)) ! kg/s3

!           add to total internal tides dissipation rate
            if (jampi == 1) then
               if (idomain(k) /= my_rank) cycle
            end if

            if (k <= Ndxi) then ! do not add fictitious boundary nodes
               DissInternalTides = DissInternalTides + DissInternalTidesPerArea(k) * ba(k) ! W
            end if
         end if
      end do
      if (ierror == 1) then
         call mess(LEVEL_ERROR, 'add_InternalTidesFrictionForces: time step too large')
      end if

!     interpolate to faces, project in face-normal direction, divide by water depth and add to adve
      do L = 1, Lnx
         if (hu(L) > 0d0) then
            k1 = ln(1, L)
            k2 = ln(2, L)

!            adve(L) = adve(L) - huvli(L) * (  &
!              (acL(L)*workx(k1) + (1d0-acL(L))*workx(k2)) * csu(L) +   &
!              (acL(L)*worky(k1) + (1d0-acL(L))*worky(k2)) * snu(L) )

            dumx1 = nod2linx(L, 1, workx(k1), worky(k1))
            dumy1 = nod2liny(L, 1, workx(k1), worky(k1))

            dumx2 = nod2linx(L, 2, workx(k2), worky(k2))
            dumy2 = nod2liny(L, 2, workx(k2), worky(k2))

            adve(L) = adve(L) - huvli(L) * &
                      ((acL(L) * dumx1 + (1d0 - acL(L)) * dumx2) * csu(L) + &
                       (acL(L) * dumy1 + (1d0 - acL(L)) * dumy2) * snu(L))

         end if
      end do

      return
   end subroutine add_InternalTidesFrictionForces

end module m_add_internaltidesfrictionforces
