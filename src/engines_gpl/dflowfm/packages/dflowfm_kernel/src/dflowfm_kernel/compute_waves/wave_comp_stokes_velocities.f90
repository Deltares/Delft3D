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

   subroutine wave_comp_stokes_velocities()
      use m_flowparameters
      use m_flowgeom
      use m_flow, only: hu, hs
      use m_physcoef, only: sag
      use m_waves
      use m_sferic
      use m_partitioninfo
      implicit none

      double precision :: Mu, Mv, massflux_max, mnorm, mangle ! link-based and link-oriented wave-induced volume fluxes
      double precision :: gammal, hwavL, hstokes, huL, deltahmin
      double precision, allocatable :: mx(:), my(:)

      integer :: k1, k2, L, k
      integer :: ierror ! error (1) or not (0)

      double precision :: ac1, ac2

      ierror = 1

      !
      ustokes = 0d0
      vstokes = 0d0

      ! switch off stokes drifts
      if (jawavestokes == 0) then
         return
      end if

      if (.not. (allocated(mx))) then
         allocate (mx(1:ndx), my(1:ndx), stat=ierror)
      end if

      deltahmin = 0.1d0 ! should be a parameter
      !
      do k = 1, ndx
         massflux_max = 1d0 / 8d0 * sag * (hs(k)**1.5) * (gammax**2)
         mnorm = min(sqrt(mxwav(k)**2 + mywav(k)**2), massflux_max)
         mangle = atan2(mywav(k), mxwav(k))
         mx(k) = mnorm * cos(mangle)
         my(k) = mnorm * sin(mangle)
      end do

      if (jampi > 0) then
         call update_ghosts(ITYPE_SALL, 1, ndx, mx, ierror)
         call update_ghosts(ITYPE_SALL, 1, ndx, my, ierror)
      end if

      do L = 1, Lnxi
         if (hu(L) > epshu) then
            !
            k1 = ln(1, L); k2 = ln(2, L)
            ac1 = acl(L); ac2 = 1d0 - ac1
            !
            ! civilized behaviour in shallow surf zone
            huL = max(hs(k1), hs(k2))
            hwavL = 0.5d0 * (hwav(k1) + hwav(k2))
            gammal = hwavL / huL
            if (gammal > 1.d0) then
               hstokes = deltahmin * (gammal - 1.d0) * hwavL + huL
            else
               hstokes = huL
            end if
            !
            Mu = ac1 * (csu(L) * (Mx(k1)) + snu(L) * (My(k1))) + &
                 ac2 * (csu(L) * (Mx(k2)) + snu(L) * (My(k2)))

            Mv = ac1 * (-snu(L) * (Mx(k1)) + csu(L) * (My(k1))) + &
                 ac2 * (-snu(L) * (Mx(k2)) + csu(L) * (My(k2)))

            ustokes(L) = Mu / hstokes
            vstokes(L) = Mv / hstokes
         else
            ustokes(L) = 0d0
            vstokes(L) = 0d0
         end if
      end do

      do L = lnxi + 1, lnx ! Randen: Neumann
         if (hu(L) > epshu) then

            k1 = ln(1, L) ! buiten
            k2 = ln(2, L) ! binnen
            !
            huL = hs(k2)
            hwavL = hwav(k2)
            gammal = hwavL / huL
            if (gammal > 1.d0) then
               hstokes = deltahmin * (gammal - 1.d0) * hwavL + huL
            else
               hstokes = huL
            end if
            Mx(k1) = Mx(k2); My(k1) = My(k2)
            !
            Mu = ac1 * (csu(L) * (Mx(k1)) + snu(L) * (My(k1))) + &
                 ac2 * (csu(L) * (Mx(k2)) + snu(L) * (My(k2)))

            Mv = ac1 * (-snu(L) * (Mx(k1)) + csu(L) * (My(k1))) + &
                 ac2 * (-snu(L) * (Mx(k2)) + csu(L) * (My(k2)))
            !
            ustokes(L) = Mu / hstokes
            vstokes(L) = Mv / hstokes
         else
            ustokes(L) = 0d0
            vstokes(L) = 0d0
         end if
      end do

      if (jampi > 0) then
         call update_ghosts(ITYPE_U, 1, lnx, ustokes, ierror)
         call update_ghosts(ITYPE_U, 1, lnx, vstokes, ierror)
         !   call update_ghostboundvals(ITYPE_U, 1, lnx, ustokes, 0, ierror)
         !   call update_ghostboundvals(ITYPE_U, 1, lnx, vstokes, 0, ierror)
      end if

      ierror = 0
1234  continue
      return
   end subroutine wave_comp_stokes_velocities
