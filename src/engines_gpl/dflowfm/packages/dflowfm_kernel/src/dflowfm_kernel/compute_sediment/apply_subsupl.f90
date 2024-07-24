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

! update the bed levels due to subsidence/uplift
subroutine apply_subsupl()
   use m_flowtimes, only: dts, dt_user
   use m_subsidence, only: sdu_blp, subsupl, subsupl_tp
   use m_flowparameters, only: ibedlevtyp
   use m_flowgeom, only: lnx, ndx, bl, blu
   use network_data, only: numk, zk

   implicit none

   integer :: k !< face/cell or node index
   integer :: L !< link/edge index

   ! copy bed level at cell centres to detect bed level changes later when updating water levels
   do k = 1, ndx
      sdu_blp(k) = bl(k)
   end do

   ! subsidence rate is interpolated in space and time in setexternalforcings() at tim=tstart+n*dt_user
   ! Where the subs/uplift is applied depends on ibedlevtyp
   ! According to setblfromextfile:
   !
   ! ibedlevtyp determines from which source data location the bed levels are used to derive bobs and bl.
   ! These types need to be mapped to one of three possible primitive locations (center/edge/corner).
   !select case (ibedlevtyp)
   !case (1)       ! position = waterlevelpoint, cell centre
   !   iprimpos = 2 ; mx = max(numk, ndx)
   !case (2)       ! position = velocitypoint, cellfacemid
   !   iprimpos = 1 ; mx = max(numk, lnx)
   !case (3,4,5,6) ! position = netnode, cell corner
   !   iprimpos = 3 ; mx = numk
   !end select
   select case (ibedlevtyp)
   case (1)
      do k = 1, ndx
         bl(k) = bl(k) + (subsupl(k) - subsupl_tp(k)) / dt_user * dts
      end do
   case (2)
      do L = 1, lnx
         blu(L) = blu(L) + (subsupl(L) - subsupl_tp(L)) / dt_user * dts
      end do
   case (3, 4, 5, 6)
      do k = 1, numk
         zk(k) = zk(k) + (subsupl(k) - subsupl_tp(k)) / dt_user * dts
      end do
   end select
end subroutine apply_subsupl
