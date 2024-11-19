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

!> Location parameter module to generalize all possible topological locations
!! at which various FM state data can be located.
!! Consists mainly of staggered grid locations and various object locations.
module fm_location_types
   implicit none
   private

   public get_location_specifier_string

   integer, parameter, public :: UNC_LOC_UNKNOWN = 0      !< Data location: unknown or invalid location.
   ! Grid locations:
   integer, parameter, public :: UNC_LOC_CN = 1           !< Data location: corner point.
   integer, parameter, public :: UNC_LOC_S = 2            !< Data location: pressure point.
   integer, parameter, public :: UNC_LOC_U = 3            !< Data location: horizontal velocity point.
   integer, parameter, public :: UNC_LOC_L = 13           !< Data location: horizontal net link.
   integer, parameter, public :: UNC_LOC_S3D = 4          !< Data location: pressure point in all layers.
   integer, parameter, public :: UNC_LOC_U3D = 5          !< Data location: horizontal velocity point in all layers.
   integer, parameter, public :: UNC_LOC_W = 6            !< Data location: vertical velocity point on all layer interfaces.
   integer, parameter, public :: UNC_LOC_WU = 16          !< Data location: vertical viscosity point on all layer interfaces.
   integer, parameter, public :: UNC_LOC_3DV = 17         !< Data location: vertical positions at pressure points and constant for 
                                                          !! all gridpoints.
   

   ! Model global:
   integer, parameter, public :: UNC_LOC_GLOBAL = 21      !< Data location: model global (e.g. water balance)

   ! Special point locations:
   integer, parameter, public :: UNC_LOC_SOSI = 22        !< Data location: source and sink
   integer, parameter, public :: UNC_LOC_GENSTRU = 23     !< Data location: general structure
   integer, parameter, public :: UNC_LOC_DAM = 24         !< Data location: controllable dam
   integer, parameter, public :: UNC_LOC_PUMP = 25        !< Data location: pump
   integer, parameter, public :: UNC_LOC_GATE = 26        !< Data location: old gate
   integer, parameter, public :: UNC_LOC_GATEGEN = 42     !< Data location: new gate
   integer, parameter, public :: UNC_LOC_WEIRGEN = 27     !< Data location: weir
   integer, parameter, public :: UNC_LOC_ORIFICE = 28     !< Data location: orifice
   integer, parameter, public :: UNC_LOC_BRIDGE = 29      !< Data location: bridge
   integer, parameter, public :: UNC_LOC_CULVERT = 30     !< Data location: culvert
   integer, parameter, public :: UNC_LOC_DAMBREAK = 31    !< Data location: dambreak
   integer, parameter, public :: UNC_LOC_UNIWEIR = 32     !< Data location: universal weir
   integer, parameter, public :: UNC_LOC_CMPSTRU = 33     !< Data location: compound structure
   integer, parameter, public :: UNC_LOC_LONGCULVERT = 34 !< Data location: long culvert
   integer, parameter, public :: UNC_LOC_STATION = 35     !< Data location: observation station
   integer, parameter, public :: UNC_LOC_OBSCRS = 36      !< Data location: observation cross section
   integer, parameter, public :: UNC_LOC_LATERAL = 37     !< Data location: lateral locations
   integer, parameter, public :: UNC_LOC_RUG = 38         !< Data location: run-up gauge
   integer, parameter, public :: UNC_LOC_DREDGE = 39      !< Data location: dredge
   integer, parameter, public :: UNC_LOC_DUMP = 40        !< Data location: dump
   integer, parameter, public :: UNC_LOC_DRED_LINK = 41   !< Data location: dredge links
contains
   !> Convert a location specifier to a human-readable string
   function get_location_specifier_string(location_specifier) result(string)
      use MessageHandling, only: mess, LEVEL_ERROR

      integer, intent(in) :: location_specifier !< The location specifier (one from the UNC_LOC_XXX parameter set).

      character(:), allocatable :: string       !< Resulting humand-readable string for the given location.

      select case (location_specifier)
      case (UNC_LOC_CN)
         string = 'corner point'
      case (UNC_LOC_S)
         string = 'pressure point'
      case (UNC_LOC_U)
         string = 'horizontal velocity point'
      case (UNC_LOC_L)
         string = 'horizontal net link'
      case (UNC_LOC_S3D)
         string = 'pressure point in all layers'
      case (UNC_LOC_U3D)
         string = 'horizontal velocity point in all layers'
      case (UNC_LOC_W)
         string = 'vertical velocity point on all layer interfaces'
      case (UNC_LOC_WU)
         string = 'vertical viscosity point on all layer interface'
      case (UNC_LOC_GLOBAL)
         string = 'global variable'
      case (UNC_LOC_SOSI)
         string = 'source/sink'
      case (UNC_LOC_GENSTRU)
         string = 'general structure'
      case (UNC_LOC_DAM)
         string = 'controllable dam'
      case (UNC_LOC_PUMP)
         string = 'pump'
      case (UNC_LOC_GATE)
         string = 'gate'
      case (UNC_LOC_WEIRGEN)
         string = 'weir'
      case (UNC_LOC_ORIFICE)
         string = 'orifice'
      case (UNC_LOC_BRIDGE)
         string = 'bridge'
      case (UNC_LOC_CULVERT)
         string = 'culvert'
      case (UNC_LOC_DAMBREAK)
         string = 'dambreak'
      case (UNC_LOC_UNIWEIR)
         string = 'universal weir'
      case (UNC_LOC_CMPSTRU)
         string = 'compound structure'
      case (UNC_LOC_LONGCULVERT)
         string = 'long culvert'
      case (UNC_LOC_STATION)
         string = 'observation station'
      case (UNC_LOC_OBSCRS)
         string = 'observation cross section'
      case (UNC_LOC_LATERAL)
         string = 'lateral location'
      case (UNC_LOC_RUG)
         string = 'run-up gauge'
      case (UNC_LOC_DREDGE)
         string = 'dredge'
      case (UNC_LOC_DUMP)
         string = 'dump'
      case (UNC_LOC_DRED_LINK)
         string = 'dredge link'
      case default
         call mess(LEVEL_ERROR, 'Programming error, please report: unrecognised location_specifier in fm_location_types::get_location_specifier_string(), location_specifier = ', location_specifier)
      end select
   end function get_location_specifier_string
end module fm_location_types
