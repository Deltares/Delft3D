!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2022-2024.
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
module m_update_fourier
   use m_fourier_analysis
   implicit none
   private

   public :: update_fourier
   public :: fourier_save_dry_wet_mask

contains

!> update module array energy head eh
   subroutine update_eh()
      use m_fourier_analysis, only: eh
      use m_flow, only: s1, ucx, ucy
      use m_flowgeom, only: ndx
      use m_alloc, only: realloc, aerr
      use m_physcoef, only: ag

      implicit none

      integer :: k !< flow node index
      integer :: ierr !< error flag
      double precision :: twogi !< 1/(2*g)

      call realloc(eh, ndx, stat=ierr)
      call aerr('eh(ndx)', ierr, ndx)

      twogi = 1d0 / (2d0 * ag)
      do k = 1, ndx
         eh(k) = s1(k) + twogi * (ucx(k)**2 + ucy(k)**2)
      end do
   end subroutine update_eh

!> update module array sul
!! first make sure the size is correct
!! then fill using getblu_from_bob
   subroutine update_wl_at_links()
      use m_fourier_analysis, only: sul
      use m_flow, only: hu
      use m_flowgeom, only: lnx
      use m_alloc, only: realloc, aerr

      implicit none

      integer :: L !< flow link index
      integer :: iup !< dummy upwind node index
      integer :: ierr !< error flag
      double precision :: blu !< bed level at flow link

      call realloc(sul, lnx, stat=ierr)
      call aerr('sul(lnx)', ierr, lnx)

      iup = 0
      do L = 1, lnx
         call getblu_from_bob(L, iup, blu)
         sul(L) = blu + hu(L)
      end do
   end subroutine update_wl_at_links

!> prepare all quantities needed by the Fourier analysis and perform Fourier computation
   subroutine update_fourier(ti_fou)
      use m_fourier_analysis
      use m_flowtimes, only: time0
      use m_flow, only: ndkx, workx, worky, ucmag, jaeulervel
      use unstruc_channel_flow, only: network
      use m_oned_functions, only: updateFreeboard, updateDepthOnGround, updateVolOnGround
      use m_get_ucx_ucy_eul_mag

      double precision, intent(in) :: ti_fou !< Fourier time step

      if (.not. fourierIsActive()) then
         return
      end if

      if (fourierWithEh()) then
         call update_eh()
      end if
      if (fourierWithUc()) then
         call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulervel, 1)
      end if
      if (fourierWithSul()) then
         call update_wl_at_links()
      end if
      if (network%loaded) then
         if (fourierWithFb()) then
            call updateFreeboard(network)
         end if
         if (fourierWithWdog()) then
            call updateDepthOnGround(network)
         end if
         if (fourierWithVog()) then
            call updateVolOnGround(network)
         end if
      end if
      call postpr_fourier(time0, ti_fou)
   end subroutine update_fourier

!> store dry/wet mask for Fourier analysis
   subroutine fourier_save_dry_wet_mask()
      use m_fourier_analysis, only: initial_wet_mask
      use m_flowgeom, only: ndx, kfs
      use m_alloc, only: realloc, aerr

      implicit none

      integer :: k !< flow link index
      integer :: ierr !< error flag

      call realloc(initial_wet_mask, ndx, stat=ierr)
      call aerr('initial_wet_mask(ndx)', ierr, ndx)

      do k = 1, ndx
         initial_wet_mask(k) = kfs(k)
      end do
   end subroutine fourier_save_dry_wet_mask

end module m_update_fourier
