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

module m_compute_wave_forcing_rhs
   use m_xbeachwaves, only: xbeach_waves, xbeach_flow_bc, xbeach_wave_compute_flowforcing2D, xbeach_apply_wave_bc, xbeach_wave_bc, xbeach_wave_compute_flowforcing3D
   use m_wave_makeplotvars, only: wave_makeplotvars
   use m_tauwave, only: tauwave
   use m_setwavmubnd, only: setwavmubnd
   use m_setwavfu, only: setwavfu

   implicit none

   private

   public :: compute_wave_forcing_rhs

contains

   subroutine compute_wave_forcing_RHS()
      use m_xbeach_data
      use m_waves
      use m_flow
      use m_flowgeom
      use m_sferic
      use m_flowtimes
      use m_xbeach_netcdf

      use unstruc_display

      implicit none

      integer :: ierror

      ! Fetch models
      !
      if (jawave < 3 .and. .not. flowWithoutWaves) then
         if (kmx == 0) then
            call tauwave() ! 3D, done in update_verticalprofiles
         end if
      end if

      ! SWAN
      if ((jawave == 3 .or. jawave >= 6) .and. .not. flowWithoutWaves) then
         if (kmx == 0) then
            call tauwave() ! 3D, done in update_verticalprofiles
         end if
         call setwavfu()
         call setwavmubnd()
      end if
      !
      ! Surfbeat model
      if (jawave == 4 .and. jajre == 1 .and. nwbnd > 0 .and. .not. flowWithoutWaves) then
         if (swave == 1) then
            call xbeach_wave_bc()
            call xbeach_apply_wave_bc()
            call xbeach_waves(ierror)
            !
            call xbeach_wave_compute_flowforcing2D() ! Always: sets fx, fy based on radiation stress gradients
            if (kmx > 0) then
               call xbeach_wave_compute_flowforcing3D() ! set wavfu 3D
            end if
            !
            if (jaavgwavquant == 1) then
               call xbeach_makeaverages(dts) ! time-averaged stats
            end if
         else
            uin = 0d0
            vin = 0d0
         end if
         !
         if (kmx == 0) then
            call tauwave()
            call xbeach_flow_bc()
         end if
      end if
      !
      ! Uniform wave field
      if (jawave == 5 .and. .not. flowWithoutWaves) then
         if (kmx == 0) then
            call tauwave()
         end if
      end if
      !
      ! this part is for online interacter visualisation
      if (jaGUI == 1 .and. jawave > 2 .and. .not. flowWithoutWaves) then
         if (ntek > 0) then
            if (mod(int(dnt), ntek) == 0) then
               call wave_makeplotvars()
            end if
         end if
      end if

1234  continue
      return
   end subroutine compute_wave_forcing_RHS

end module m_compute_wave_forcing_rhs
