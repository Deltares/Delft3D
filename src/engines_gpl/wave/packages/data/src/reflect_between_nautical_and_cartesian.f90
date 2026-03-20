module angle_convention
   !----- GPL ---------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2011-2026.
   !
   !  This program is free software: you can redistribute it and/or modify
   !  it under the terms of the GNU General Public License as published by
   !  the Free Software Foundation version 3.
   !
   !  This program is distributed in the hope that it will be useful,
   !  but WITHOUT ANY WARRANTY; without even the implied warranty of
   !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   !  GNU General Public License for more details.
   !
   !  You should have received a copy of the GNU General Public License
   !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
   !
   !  contact: delft3d.support@deltares.nl
   !  Stichting Deltares
   !  P.O. Box 177
   !  2600 MH Delft, The Netherlands
   !
   !  All indications and logos of, and references to, "Delft3D" and "Deltares"
   !  are registered trademarks of Stichting Deltares, and remain the property of
   !  Stichting Deltares. All rights reserved.
   !
   !-------------------------------------------------------------------------------
contains
   !! \brief Convert angle between Cartesian and Nautical conventions.
   !!
   !! Converts an angle in degrees from Cartesian convention to Nautical convention
   !! or vice versa, and normalises the result to the range [0, 360).
   !!
   !! Cartesian convention: 0 degrees points to the east and 90 degrees to the north.
   !! Nautical convention : 0 degrees is from the north and 90 degrees from the east.
   !!
   !! The relation used (both directions use the same formula):
   !! - cart => naut: naut = 180.0 + north_direction - cart
   !! - naut => cart: cart = 180.0 + north_direction - naut
   !!
   !! @param[in] angle_in        Angle in degrees, given in either Cartesian or Nautical convention.
   !! @param[in] north_direction Direction of geographic north in degrees; measured clockwise from east.
   !! @return angle_out          Angle in degrees in the opposite convention to `angle_in`, normalized to [0,360).
   !! @note This is an elemental function and therefore applies element-wise to arrays.
   elemental function reflect_between_nautical_and_cartesian(angle_in, north_direction) result(angle_out)
      implicit none
      real, intent(in) :: angle_in !> In degrees, either Cartesian or Nautical convention
      real, intent(in) :: north_direction !> Direction of north in degrees (clockwise from east)
      real :: angle_out !> In degrees, either Nautical or Cartesian  convention, depending on the input
      !
      angle_out = MODULO(180.0 + north_direction - angle_in, 360.0)
   end function reflect_between_nautical_and_cartesian
end module angle_convention
