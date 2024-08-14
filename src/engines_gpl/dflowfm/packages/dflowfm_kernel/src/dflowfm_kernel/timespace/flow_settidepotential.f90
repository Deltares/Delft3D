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

 subroutine flow_settidepotential(timmin)
    use m_flow
    use m_flowgeom
    use m_flowtimes
    use timespace_data
    use m_sferic
    use unstruc_model
    use m_equatorial

    implicit none

    double precision :: timmin
    integer :: kk
    double precision :: tt

    call meteo_tidepotential(julrefdat, TIMmin, doodsonstart, doodsonstop, doodsoneps)

    if (md_ident == 'equator1d') then
       tt = 60d0 * timmin - tstart_user
       do kk = 1, ndx
          tidep(1, kk) = ZP * sin(om * tt - nmode * dg2rd * xz(kk))
       end do
    end if

 end subroutine flow_settidepotential
