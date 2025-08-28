!----- LGPL --------------------------------------------------------------------
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
subroutine wave_forces(dir, tp, fxhis, &
                         & fyhis, dish, diss, wavel, &
                         & water_is_too_shallow_or_waves_are_too_small, &
                         & fx, fy, &
                         & swdis, grav, wsbodyx, wsbodyy)
    !!--description-----------------------------------------------------------------
   !
   !     Input:
   !     -------
   !     DIR,TP,FXHIS,FYHIS,DISH,DISS,WAVEL,water_is_too_shallow_or_waves_are_too_small,SWDIS,grav
   !     water_is_too_shallow_or_waves_are_too_small   : logical variable, .true. when depth or waveheight is too small
   !     SWDIS  : 1: wave forces based on gradients,
   !              2: from total dissipation,
   !              3: using the 3D dissipation profile
   !
   !     Output:
   !     --------
   !     FX,FY,wsbodyx,wsbodyy
   !     FX,FY : wave forces based on gradients(1) or total dissipation(2) or 3d dissipation(3)
   !             Unit: N/m2
   !
    !!--pseudo code and references--------------------------------------------------
   ! NONE
    !!--declarations----------------------------------------------------------------
   use precision
   implicit none
   !
   ! Global variables
   !
   logical, intent(in) :: water_is_too_shallow_or_waves_are_too_small
   integer, intent(in) :: swdis
   real(hp), intent(in) :: dir
   real(hp), intent(in) :: dish
   real(hp), intent(in) :: diss
   real(hp), intent(out) :: fx
   real(hp), intent(in) :: fxhis
   real(hp), intent(out) :: fy
   real(hp), intent(in) :: fyhis
   real(hp), intent(in) :: grav
   real(hp), intent(in) :: tp
   real(hp), intent(in) :: wavel
   real(hp), intent(out) :: wsbodyx
   real(hp), intent(out) :: wsbodyy
   !
   ! Local variables
   !
   real(hp) :: frc
   real(hp) :: tr_angle
   !
    !! executable statements -------------------------------------------------------
   !
   if (water_is_too_shallow_or_waves_are_too_small) then
      fx = 0.0
      fy = 0.0
      wsbodyx = 0.0
      wsbodyy = 0.0
   else
      if (swdis == 1) then
         !
         ! Determine wave forces based on gradients radiation stresses
         !
         fx = fxhis
         fy = fyhis
         wsbodyx = 0.0
         wsbodyy = 0.0
      elseif (swdis == 2) then
         !
         ! Determine wave forces based on dissipation
         !
         frc = dish * tp / wavel
         !
         tr_angle = 0.0174533 * dir
         fx = cos(tr_angle) * frc
         fy = sin(tr_angle) * frc
         wsbodyx = 0.0
         wsbodyy = 0.0
      elseif (swdis == 3) then
         !
         ! Determine wave forces based on dissipation at the free surface
         ! and a separate contribution to the water column through the
         ! remaining forces (wsbodyu/wsbodyv)
         !
         frc = diss * tp / wavel
         !
         tr_angle = 0.0174533 * dir
         fx = cos(tr_angle) * frc
         fy = sin(tr_angle) * frc
         wsbodyx = fxhis - fx
         wsbodyy = fyhis - fy
      else
         fx = 0.0
         fy = 0.0
         wsbodyx = 0.0
         wsbodyy = 0.0
      end if
   end if
end subroutine wave_forces
