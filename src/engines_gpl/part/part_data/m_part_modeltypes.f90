!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module m_part_modeltypes

    !  module declarations
    implicit none
    integer, parameter :: model_tracers = 1
    integer, parameter :: model_two_layer_temp = 2
    integer, parameter :: model_red_tide = 3
    integer, parameter :: model_oil = 4
    integer, parameter :: model_2d3d_temp = 5
    integer, parameter :: model_prob_dens_settling = 6
    integer, parameter :: model_abm = 7
end module m_part_modeltypes
