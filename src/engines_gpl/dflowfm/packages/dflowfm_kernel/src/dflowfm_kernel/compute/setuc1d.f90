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
module m_setuc1d

   implicit none

   private

   public :: setuc1d

contains

   subroutine setuc1d()
      use m_netw
      use m_flow
      use m_flowgeom
      use m_get_prof_1D
      use precision, only: dp
      implicit none

      integer, parameter :: JACSTOT = 0 !< 0 for computing the total area
      integer, parameter :: JACSFLW = 1 !< 1 for computing the flow area
      integer, parameter :: CALCCONV = 0 !< don't update wu, cfuhi, etc inside getprof_1D

      integer :: L, LL, La, n, nx, ja1D

      real(kind=dp) :: q_net_in !< [m3/s] sum of inflowing Q minus sum of outflowing Q over links of node n
      real(kind=dp) :: q_in !< [m3/s] sum of inflowing Q over links of node n
      real(kind=dp) :: q_out !< [m3/s] sum of outflowing Q over links of node n
      real(kind=dp) :: qu_in !< [m4/s2] sum of Q*u over inflowing links of node n
      real(kind=dp) :: qu_out !< [m4/s2] sum of Q*u over outflowing links of node n (u = Q/A)
      real(kind=dp) :: qu2_in !< [m5/s3] sum of Q*u**2 over inflowing links of node n
      real(kind=dp) :: qu2_out !< [m5/s3] sum of Q*u**2 over outflowing links of node n (u = Q/A)
      real(kind=dp) :: uc !< [m/s] representative velocity magnitude at node n

      integer :: L1 !< index of first link
      integer :: k !< node index: 1 for link start node, and 2 for link end node
      real(kind=dp) :: h !< [m] local water depth
      real(kind=dp) :: half_link_length !< [m] half link length
      real(kind=dp) :: u !< [m/s] velocity
      real(kind=dp) :: q !< [m3/s] discharge
      real(kind=dp) :: perim !< [m] dummy variable for wetted perimeter
      real(kind=dp) :: flow_cs_area !< [m2] cross-sectional flow area
      real(kind=dp) :: total_cs_area !< [m2] cross-sectional total (flow + storage) area
      real(kind=dp) :: link_surface_area !< [m2] surface area of half link
      real(kind=dp) :: surface_area !< [m2] total surface area of node -- equal to a1(n)
      real(kind=dp) :: flow_width !< [m] surface width of flow area
      real(kind=dp) :: total_width !< [m] surface width of total (flow + storage) area
      real(kind=dp) :: dzw_dt !< [m/s] water level change rate

      if (kmx /= 0 .or. lnx1D == 0) return

      uc1D = 0d0
      do n = ndx2D + 1, ndxi
         nx = nd(n)%lnx

         ja1D = 1
         do LL = 1, nx
            L = nd(n)%ln(LL)
            La = abs(L)
            if (abs(kcu(La)) /= 1) ja1D = 0
         end do
         if (ja1D == 0) cycle
         if (jaJunction1D == 0 .and. nx > 2) cycle

         qu_in = 0d0
         qu_out = 0d0
         q_in = 0d0
         q_out = 0d0
         do LL = 1, nx ! loop over all links of the upstream node
            L = nd(n)%ln(LL) ! positive if link points to node, negative if links points from node
            La = abs(L)

            if (L * u1(La) >= 0d0) then ! inflowing: positive flow to this node, or negative flow from this node
               qu_in = qu_in + qa(La) * u1(La)
               q_in = q_in + abs(qa(La))
            else ! outflowing: positive flow from this node, or negative flow to this node
               qu_out = qu_out + qa(La) * u1(La)
               q_out = q_out + abs(qa(La))
            end if
         end do

         if (q_in > 0d0 .and. q_out > 0d0) then
            uc = 0.5d0 * (qu_in / q_in + qu_out / q_out)
         else ! all inflow, all outflow, or stagnant
            uc = 0d0
         end if

         L1 = abs(nd(n)%ln(1))
         uc1D(n) = sign(uc, u1(L1))
      end do

      do LL = lnxi + 1, lnx ! loop over open boundary links
         if (kcu(LL) == -1) then ! 1D boundary link
            n = Ln(1, LL)

            ! a 1D boundary node has just one link (the boundary link)
            ! so the sign of the node is equal to the sign of the link
            uc1D(n) = u1(LL)
         end if
      end do

      if (jaPure1D == 1 .or. jaPure1D == 2) then
         u1Du = 0d0
         do L = 1, lnx
            if (qa(L) > 0 .and. abs(uc1D(ln(1, L))) > 0) then ! set upwind ucxu, ucyu  on links
               u1Du(L) = uc1D(ln(1, L))
            else if (qa(L) < 0 .and. abs(uc1D(ln(2, L))) > 0) then
               u1Du(L) = uc1D(ln(2, L))
            end if
         end do

      elseif (jaPure1D >= 3) then

         q1D = 0d0
         au1D = 0d0
         sar1D = 0d0
         volu1D = 0d0
         alpha_mom_1D = 0d0
         alpha_ene_1D = 0d0
         do n = ndx2D + 1, ndxi
            nx = nd(n)%lnx

            ja1D = 1
            do LL = 1, nx
               L = nd(n)%ln(LL)
               La = abs(L)
               if (abs(kcu(La)) /= 1) ja1D = 0
            end do
            if (ja1D == 0) cycle
            if (jaJunction1D == 0 .and. nx > 2) cycle

            ! compute total net discharge into the node
            q_net_in = 0d0
            surface_area = 0d0
            do LL = 1, nx ! loop over all links connected to the node
               L = nd(n)%ln(LL) ! positive if link points to node, negative if links points from node
               La = abs(L)

               half_link_length = 0.5 * dx(La)
               if (L > 0) then ! link points to node
                  k = 2
               else ! link points from node
                  k = 1
               end if

               h = max(0d0, s1(n) - bob(k, La)) ! cross sectional area
               call getprof_1D(La, h, total_cs_area, total_width, JACSTOT, CALCCONV, perim)
               call getprof_1D(La, h, flow_cs_area, flow_width, JACSFLW, CALCCONV, perim)
               link_surface_area = total_width * half_link_length
               wu1D(k, La) = total_width
               au1D(k, La) = flow_cs_area
               sar1D(k, La) = link_surface_area

               surface_area = surface_area + link_surface_area
               volu1D(La) = volu1D(La) + flow_cs_area * half_link_length

               q_net_in = q_net_in + dble(sign(1, L)) * qa(La)
            end do

            qu_in = 0d0
            qu_out = 0d0
            qu2_in = 0d0
            qu2_out = 0d0
            dzw_dt = q_net_in / surface_area
            do LL = 1, nx ! loop over all links connected to the node
               L = nd(n)%ln(LL) ! positive if link points to node, negative if links points from node
               La = abs(L)

               if (L > 0) then ! link points to node: reduce by the storage on the remainder of the link
                  link_surface_area = sar1D(2, La)
                  flow_cs_area = au1D(2, La)
                  q = qa(La) - link_surface_area * dzw_dt
                  q1D(2, La) = q
               else ! link points from node: increase by the storage on the first part of the link
                  link_surface_area = sar1D(1, La)
                  flow_cs_area = au1D(1, La)
                  q = qa(La) + link_surface_area * dzw_dt
                  q1D(1, La) = q
               end if

               if ((L * q) >= 0d0) then ! inflowing: positive flow to this node, or negative flow from this node
                  u = u1(La)
                  if ((q * u) > 0) then ! flow direction at link equal to flow direction at node
                     qu_in = qu_in + abs(q * u)
                     qu2_in = qu2_in + abs(q * u**2)
                  else ! flow direction at link opposite to flow direction at node, so use 0 velocity inflow
                     ! no contribution if u = 0
                  end if
               elseif (flow_cs_area > 0d0) then ! outflowing: negative flow to this node, or positive flow from this node
                  u = q / flow_cs_area
                  qu_out = qu_out + abs(q * u)
                  qu2_out = qu2_out + abs(q * u**2)
               end if
            end do

            alpha_mom_1D(n) = qu_in / max(1e-20_dp, qu_out)
            alpha_ene_1D(n) = qu2_in / max(1e-20_dp, qu2_out)
         end do
      end if

   end subroutine setuc1d

end module m_setuc1d
