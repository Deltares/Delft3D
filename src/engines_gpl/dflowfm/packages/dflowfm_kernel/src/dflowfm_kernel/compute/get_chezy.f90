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
module m_get_chezy
   implicit none

   private

   public :: get_chezy
contains
   !> Get the Chezy coefficient
   !! This routine is not safe for friction_coef == 0
   pure function get_chezy(radius, friction_coef, perpendicular_velocity, tangential_velocity, friction_type) result(chezy)
      use m_roughness, only: R_CHEZY, R_MANNING, R_WALL_LAW_NIKURADSE, R_WHITE_COLEBROOK
      use m_physcoef, only: sag, vonkar
      use mathconsts, only: ee
      use m_hydraulicallysmooth, only: hydraulicallysmooth
      use precision, only: dp

      real(kind=dp), intent(in) :: radius !< hydraulic radius
      real(kind=dp), intent(in) :: friction_coef !< friction coeff
      real(kind=dp), intent(in) :: perpendicular_velocity !< velocity perpendicular to the cell edge
      real(kind=dp), intent(in) :: tangential_velocity !< velocity tangential to the cell edge
      integer, intent(in) :: friction_type !< friction type
      real(kind=dp) :: chezy !< Computed Chezy coefficient

      real(kind=dp) :: floored_radius
      real(kind=dp) :: hurou, sqcf, z0, umod
      real(kind=dp), parameter :: sixth = 1.0_dp / 6.0_dp

      floored_radius = max(radius, 1e-4_dp)

      select case (friction_type)
      case (R_CHEZY)
         chezy = friction_coef
      case (R_MANNING, 4, 5, 6) ! manning, just testing implicitness in furu
         chezy = (floored_radius**sixth) / friction_coef
      case (R_WALL_LAW_NIKURADSE) ! This function does not match the description in the manual exactly
         z0 = min(friction_coef / 30.0_dp, floored_radius * 0.3_dp)
         sqcf = vonkar / log(floored_radius / (ee * z0))
         chezy = sag / sqcf
      case (R_WHITE_COLEBROOK)
         hurou = max(0.5_dp, floored_radius / friction_coef)
         chezy = 18.0_dp * log10(12.0_dp * hurou)
      case default
         umod = norm2([perpendicular_velocity, tangential_velocity])
         sqcf = hydraulicallysmooth(umod, floored_radius)
         if (sqcf > 0.0_dp) then
            chezy = sag / sqcf
         else
            chezy = 0.0_dp
         end if
      end select
   end function get_chezy
end module m_get_chezy
