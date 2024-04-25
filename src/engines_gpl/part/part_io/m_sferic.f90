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

 module m_sferic_part
 implicit none
 integer                           :: jsferic = 0       ! xy pair is in : 0=cart, 1=sferic coordinates
 integer                           :: jsfer_old         ! xy pair is in : 0=cart, 1=sferic coordinates, original model
 integer                           :: jsfertek= 0       ! drawn in 0=cart, 1=stereografisch
 integer                           :: jglobe  = 0       ! if (jsferic==1) do we need extra tests for 360-0 transgression
 double precision                  :: ptref             ! reference latitude (for testing)
 double precision                  :: pi                ! pi
 double precision                  :: twopi             ! 2pi
 double precision                  :: dg2rd             ! degrees to radians
 double precision                  :: rd2dg             ! and vice versa
 double precision                  :: ra = 6378137d0    ! earth radius (m)
 double precision                  :: omega             ! earth angular velocity (rad/s)
 double precision                  :: fcorio            ! 2omegasinfi
 double precision                  :: anglat = 0d0      ! 26.0     ! dubai 52.5     ! angle of latitude
 double precision                  :: dy2dg             ! from dy in m to lat in degrees
 double precision                  :: csphi             ! cosphi of latest requested
 end module m_sferic_part
