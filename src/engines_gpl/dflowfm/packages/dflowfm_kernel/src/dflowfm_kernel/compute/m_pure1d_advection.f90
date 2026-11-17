!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
module m_pure1d_advection

   ! Comparison reference: M. Borsboom, "Junction advection", 2017-12-29,
   ! junctionadvection.tex SVN 65523. Equation numbers below refer to that memo.
   ! The junction scheme discussed there corresponds to jaPure1D >= 3;
   ! the uc1D/u1Du path for options 1 and 2 is a separate FM formulation.

   implicit none

   private

   public :: setuc1d
   public :: advect_pure1d_sobek

contains

   !> Set representative node and upwind link velocities for jaPure1D options 1 and 2.
   subroutine setuc1d_perot()
      use m_flow, only: jaJunction1D, uc1D, u1, qa, u1Du
      use m_flowgeom, only: ndx2D, ndxi, nd, kcu, lnxi, lnx, ln
      use network_data, only: LINK_1D_BOUNDARY
      use precision, only: dp
      implicit none

      integer :: L, LL, La, n, nx, ja1D
      integer :: L1 !< index of first link
      real(kind=dp) :: q_in !< [m3/s] sum of inflowing Q over links of node n
      real(kind=dp) :: q_out !< [m3/s] sum of outflowing Q over links of node n
      real(kind=dp) :: qu_in !< [m4/s2] sum of Q*u over inflowing links of node n
      real(kind=dp) :: qu_out !< [m4/s2] sum of Q*u over outflowing links of node n (u = Q/A)
      real(kind=dp) :: uc !< [m/s] representative velocity magnitude at node n

      uc1D = 0.0_dp
      do n = ndx2D + 1, ndxi
         nx = nd(n)%lnx

         ja1D = 1
         do LL = 1, nx
            L = nd(n)%ln(LL)
            La = abs(L)
            if (abs(kcu(La)) /= 1) then
               ja1D = 0
            end if
         end do
         if (ja1D == 0) then
            cycle
         end if
         if (jaJunction1D == 0 .and. nx > 2) then
            cycle
         end if

         qu_in = 0.0_dp
         qu_out = 0.0_dp
         q_in = 0.0_dp
         q_out = 0.0_dp
         do LL = 1, nx ! loop over all links of the upstream node
            L = nd(n)%ln(LL) ! positive if link points to node, negative if links points from node
            La = abs(L)

            if (L * u1(La) >= 0.0_dp) then ! inflowing: positive flow to this node, or negative flow from this node
               qu_in = qu_in + qa(La) * u1(La)
               q_in = q_in + abs(qa(La))
            else ! outflowing: positive flow from this node, or negative flow to this node
               qu_out = qu_out + qa(La) * u1(La)
               q_out = q_out + abs(qa(La))
            end if
         end do

         if (q_in > 0.0_dp .and. q_out > 0.0_dp) then
            uc = 0.5_dp * (qu_in / q_in + qu_out / q_out)
         else ! all inflow, all outflow, or stagnant
            uc = 0.0_dp
         end if

         L1 = abs(nd(n)%ln(1))
         uc1D(n) = sign(uc, u1(L1))
      end do

      do LL = lnxi + 1, lnx ! loop over open boundary links
         if (kcu(LL) == LINK_1D_BOUNDARY) then ! 1D boundary link
            n = Ln(1, LL)

            ! a 1D boundary node has just one link (the boundary link)
            ! so the sign of the node is equal to the sign of the link
            uc1D(n) = u1(LL)
         end if
      end do

      u1Du = 0.0_dp
      do L = 1, lnx
         if (qa(L) > 0 .and. abs(uc1D(ln(1, L))) > 0) then ! set upwind ucxu, ucyu  on links
            u1Du(L) = uc1D(ln(1, L))
         else if (qa(L) < 0 .and. abs(uc1D(ln(2, L))) > 0) then
            u1Du(L) = uc1D(ln(2, L))
         end if
      end do
   end subroutine setuc1d_perot

   subroutine setuc1d()
      use m_flow, only: kmx, jaPure1D
      use m_flowgeom, only: lnx1D
      implicit none

      if (kmx /= 0 .or. lnx1D == 0) then
         return
      end if
      if (jaPure1D == 0) then
         return
      end if
      if (jaPure1D == 1 .or. jaPure1D == 2) then
         call setuc1d_perot()
         return
      end if

      call prepare_1d_advection()
   end subroutine setuc1d

   !> Prepare lateral corrections and junction fluxes for the SOBEK advection scheme.
   subroutine prepare_1d_advection()
      use m_flow, only: jaJunction1D, qin, vol0_f, vol1_f, au, &
                        q1D, au1D, sar1D, volu1D, alpha_mom_1D, alpha_ene_1D, lateral_correction, &
                        volume_change
      use m_flowgeom, only: ndx2D, ndxi, nd, kcu, dx
      use m_flowparameters, only: eps3
      use m_flowtimes, only: dts, tstart_user, time1
      use precision, only: dp
      implicit none

      integer :: L, LL, La, n, nx, ja1D

      real(kind=dp) :: q_net_in !< [m3/s] sum of inflowing Q minus sum of outflowing Q over links of node n
      real(kind=dp) :: qu_in !< [m4/s2] sum of Q*u over inflowing links of node n
      real(kind=dp) :: qu_out !< [m4/s2] sum of Q*u over outflowing links of node n (u = Q/A)
      real(kind=dp) :: qu2_in !< [m5/s3] sum of Q*u**2 over inflowing links of node n
      real(kind=dp) :: qu2_out !< [m5/s3] sum of Q*u**2 over outflowing links of node n (u = Q/A)

      real(kind=dp) :: surface_area !< [m2] total surface area of node -- equal to a1(n)

      call prepare_lateral_correction(ndx2D, ndxi, nd, qin, volume_change, vol1_f, vol0_f, &
                                      dts, time1, tstart_user, au, dx, eps3, lateral_correction)

      q1D = 0.0_dp
      au1D = 0.0_dp
      sar1D = 0.0_dp
      volu1D = 0.0_dp
      alpha_mom_1D = 0.0_dp
      alpha_ene_1D = 0.0_dp
      do n = ndx2D + 1, ndxi
         nx = nd(n)%lnx

         ! Temporary variable to check if all connected links are 1D
         ! 1 if all links are 1D, 0 otherwise
         ja1D = 1
         do LL = 1, nx
            L = nd(n)%ln(LL)
            La = abs(L)
            if (abs(kcu(La)) /= 1) then
               ja1D = 0
            end if
         end do
         if (ja1D == 0) then
            cycle
         end if
         if (jaJunction1D == 0 .and. nx > 2) then
            cycle
         end if

         call prepare_node_link_geometry(n, q_net_in, surface_area)

         call prepare_node_fluxes(n, q_net_in, surface_area, qu_in, qu_out, qu2_in, qu2_out)

         ! Sections 3.4.1-3.4.2 propose a small floor on the incoming flux
         ! balance; here a fixed 1e-20 floor is applied to outgoing sums.
         ! For eqs. (21)-(22), a velocity multiplier would be sqrt of the
         ! energy-flux ratio. This implementation uses the ratio itself;
         ! together with the Q/V weighting below, it is not the memo's
         ! energy-conserving discretization.
         alpha_mom_1D(n) = qu_in / max(1e-20_dp, qu_out)
         alpha_ene_1D(n) = qu2_in / max(1e-20_dp, qu2_out)
      end do

   end subroutine prepare_1d_advection

   !> Update connected half-link geometry and compute the node's net discharge and surface area.
   subroutine prepare_node_link_geometry(n, q_net_in, surface_area)
      use m_flow, only: qa, s1, wu1D, au1D, sar1D, volu1D
      use m_flowgeom, only: nd, dx, bob
      use m_get_prof_1D, only: getprof_1D
      use precision, only: dp
      implicit none

      integer, intent(in) :: n !< Node index.
      real(kind=dp), intent(out) :: q_net_in !< [m3/s] Net discharge into the node.
      real(kind=dp), intent(out) :: surface_area !< [m2] Total surface area of the node.

      integer, parameter :: JACSTOT = 0 !< Compute the total area.
      integer, parameter :: JACSFLW = 1 !< Compute the flow area.
      integer, parameter :: CALCCONV = 0 !< Do not calculate conveyance.
      integer :: L, LL, La, k
      real(kind=dp) :: h, half_link_length, perim
      real(kind=dp) :: total_cs_area, total_width, flow_cs_area, flow_width, link_surface_area

      q_net_in = 0.0_dp
      surface_area = 0.0_dp
      do LL = 1, nd(n)%lnx
         L = nd(n)%ln(LL) ! positive if link points to node, negative if link points from node
         La = abs(L)

         half_link_length = 0.5_dp * dx(La)
         if (L > 0) then
            k = 2
         else
            k = 1
         end if

         h = max(0.0_dp, s1(n) - bob(k, La))
         call getprof_1D(La, h, total_cs_area, total_width, JACSTOT, CALCCONV, perim)
         call getprof_1D(La, h, flow_cs_area, flow_width, JACSFLW, CALCCONV, perim)
         link_surface_area = total_width * half_link_length
         wu1D(k, La) = total_width
         au1D(k, La) = flow_cs_area
         sar1D(k, La) = link_surface_area

         surface_area = surface_area + link_surface_area
         volu1D(La) = volu1D(La) + flow_cs_area * half_link_length
         q_net_in = q_net_in + real(sign(1, L), kind=dp) * qa(La)
      end do
   end subroutine prepare_node_link_geometry

   !> Reconstruct node-end discharges and sum incoming and outgoing momentum and energy fluxes.
   subroutine prepare_node_fluxes(n, q_net_in, surface_area, qu_in, qu_out, qu2_in, qu2_out)
      use m_flow, only: u1, qa, q1D, au1D, sar1D
      use m_flowgeom, only: nd
      use precision, only: dp
      implicit none

      integer, intent(in) :: n !< Node index.
      real(kind=dp), intent(in) :: q_net_in !< [m3/s] Net discharge into the node.
      real(kind=dp), intent(in) :: surface_area !< [m2] Total surface area of the node.
      real(kind=dp), intent(out) :: qu_in !< [m4/s2] Incoming momentum flux.
      real(kind=dp), intent(out) :: qu_out !< [m4/s2] Outgoing momentum flux.
      real(kind=dp), intent(out) :: qu2_in !< [m5/s3] Incoming energy flux.
      real(kind=dp), intent(out) :: qu2_out !< [m5/s3] Outgoing energy flux.

      integer :: L, LL, La
      real(kind=dp) :: u, q, flow_cs_area, link_surface_area
      real(kind=dp) :: dzw_dt !< [m/s] Water level change rate.

      qu_in = 0.0_dp
      qu_out = 0.0_dp
      qu2_in = 0.0_dp
      qu2_out = 0.0_dp
      ! Eq. (9) removes an equal 1/K share of the signed discharge
      ! imbalance from each branch. Here the share is sar1D/surface_area:
      ! q_k = Q_k - s_k*(S_k/sum(S))*sum(s_l*Q_l).
      ! This reduces to eq. (9) only for equal half-link surface areas.
      ! Unlike continuity eqs. (4) and (17), this reconstruction excludes
      ! lateral discharge; its momentum effect is handled separately.
      dzw_dt = q_net_in / surface_area
      do LL = 1, nd(n)%lnx ! loop over all links connected to the node
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

         if ((L * q) >= 0.0_dp) then ! inflowing: positive flow to this node, or negative flow from this node
            u = u1(La)
            ! Eq. (18) uses the link velocity even during flow reversal;
            ! section 3.4.1 allows negative Q*u contributions. Here an
            ! opposing link velocity contributes neither momentum nor
            ! energy, rather than a signed momentum flux.
            if ((q * u) > 0) then ! flow direction at link equal to flow direction at node
               qu_in = qu_in + abs(q * u)
               qu2_in = qu2_in + abs(q * u**2)
            else ! flow direction at link opposite to flow direction at node, so use 0 velocity inflow
               ! no contribution if u = 0
            end if
         elseif (flow_cs_area > 0.0_dp) then ! outflowing: negative flow to this node, or positive flow from this node
            u = q / flow_cs_area
            qu_out = qu_out + abs(q * u)
            qu2_out = qu2_out + abs(q * u**2)
         end if
      end do
   end subroutine prepare_node_fluxes

   !> Compute link lateral/storage corrections without modifying global state.
   pure subroutine prepare_lateral_correction(ndx2D, ndxi, nd, qin, volume_change, vol1_f, vol0_f, &
                                             dts, time1, tstart_user, au, dx, eps3, lateral_correction)
      use m_flowgeom, only: tnode
      use precision, only: dp
      implicit none

      integer, intent(in) :: ndx2D
      integer, intent(in) :: ndxi
      type(tnode), dimension(:), intent(in) :: nd
      real(kind=dp), dimension(:), intent(in) :: qin
      real(kind=dp), dimension(:), intent(in) :: volume_change
      real(kind=dp), dimension(:), intent(in) :: vol1_f
      real(kind=dp), dimension(:), intent(in) :: vol0_f
      real(kind=dp), intent(in) :: dts
      real(kind=dp), intent(in) :: time1
      real(kind=dp), intent(in) :: tstart_user
      real(kind=dp), dimension(:), intent(in) :: au
      real(kind=dp), dimension(:), intent(in) :: dx
      real(kind=dp), intent(in) :: eps3
      real(kind=dp), dimension(:), intent(out) :: lateral_correction

      integer :: L, LL, La, n, nx
      real(kind=dp) :: lateral_flow !< [m3/s] lateral flow into the node

      lateral_correction = 0.0_dp
      if (time1 > tstart_user) then
         ! Unlike the half-cell lateral/storage terms in eqs. (8) and (16),
         ! the nodal net lateral flow is shared equally over nx links and
         ! divided by each link's au*dx, not by volu1D. The storage change is
         ! taken from timestep volume differences; no correction is made
         ! at time1 <= tstart_user.
         ! calculate lateral correction at the flow links
         lateral_flow = 0.0_dp
         do n = ndx2D + 1, ndxi
            nx = nd(n)%lnx
            ! Lateral discharge into the node
            lateral_flow = qin(n)
            ! change in storage volume storage volume = (vol1(n) - vol1_f(n))
            ! a positive change in storage volume means that the node is storing water, so the lateral flow into the node is reduced.
            ! volume_change is the total volume of a cell. And is calculated in flow_f0isf1.
            ! vol1_f and vol0_f are flow volumes in a cell and these are up to date at the current time step.
            lateral_flow = lateral_flow - ((volume_change(n) - vol1_f(n) + vol0_f(n))) / dts
            do LL = 1, nx ! loop over all links of the upstream node
               L = nd(n)%ln(LL) ! positive if link points to node, negative if links points from node
               La = abs(L)
               if (au(La) > eps3) then
                  lateral_correction(La) = lateral_correction(La) + lateral_flow / (real(nx, kind=dp) * au(La) * dx(La))
               end if

            end do
         end do
      end if

   end subroutine prepare_lateral_correction

   pure subroutine advect_pure1d_sobek(jaPure1D, u1, au1d1, au1d2, q1d1, q1d2, volu1d, alpha_mom1, alpha_ene1, &
                                       alpha_mom2, alpha_ene2, lateral_correction, advi, advel)
      use precision, only: dp
      implicit none

      integer, intent(in) :: jaPure1D
      real(kind=dp), intent(in) :: u1
      real(kind=dp), intent(in) :: au1d1
      real(kind=dp), intent(in) :: au1d2
      real(kind=dp), intent(in) :: q1d1
      real(kind=dp), intent(in) :: q1d2
      real(kind=dp), intent(in) :: volu1d
      real(kind=dp), intent(in) :: alpha_mom1
      real(kind=dp), intent(in) :: alpha_ene1
      real(kind=dp), intent(in) :: alpha_mom2
      real(kind=dp), intent(in) :: alpha_ene2
      real(kind=dp), intent(in) :: lateral_correction
      real(kind=dp), intent(inout) :: advi
      real(kind=dp), intent(inout) :: advel

      real(kind=dp) :: am
      real(kind=dp) :: qv
      real(kind=dp) :: u_ene
      real(kind=dp) :: u_mom

      advel = 0.0_dp
      ! Section 3.3 assigns pure1D=5 to energy mode. Here that is option 7;
      ! options 5 and 6 instead restrict the area-ratio mix to contractions
      ! and expansions, respectively. Unlike the memo's unregularized area
      ! ratio, the mixed options use a 1e-4 lower bound on its denominator.
      select case (jaPure1D)
      case (3) ! momentum conserving
         am = 1.0_dp
      case (4) ! weighted
         am = min(au1d1, au1d2) / max(1.0e-4_dp, au1d1, au1d2)
      case (5) ! weighted in contractions, otherwise momentum conserving
         if ((u1 > 0.0_dp .and. au1d1 > au1d2) .or. (u1 < 0.0_dp .and. au1d1 < au1d2)) then
            am = min(au1d1, au1d2) / max(1.0e-4_dp, au1d1, au1d2)
         else
            am = 1.0_dp
         end if
      case (6) ! weighted in expansions, otherwise momentum conserving
         if ((u1 > 0.0_dp .and. au1d1 < au1d2) .or. (u1 < 0.0_dp .and. au1d1 > au1d2)) then
            am = min(au1d1, au1d2) / max(1.0e-4_dp, au1d1, au1d2)
         else
            am = 1.0_dp
         end if
      case (7) ! energy conserving
         am = 0.0_dp
      end select

      ! Eq. (16) mixes Q/V and averaged velocity/dx as transport rates.
      ! Here both parts use Q/max(1e-5,volu1d), mixing reconstructed velocities
      ! instead. Eqs. (20) and (22) print Q/V for those velocities; Q/A is used
      ! here, consistent with velocity units and the memo's "flow speed" text.
      ! The same formulation is applied at both link ends.
      if (q1d1 > 0.0_dp) then
         qv = q1d1 / max(1.0e-5_dp, volu1d)
         u_mom = alpha_mom1 * q1d1 / au1d1
         u_ene = alpha_ene1 * q1d1 / au1d1
         advel = advel - am * (u_mom - u1) * qv - (1.0_dp - am) * (u_ene - u1) * qv
      end if

      ! The lateral/storage sink is implicit (advi), unlike advection (advel).
      ! Contributions from both nodes are summed before taking the positive
      ! part, so opposing lateral/storage exchanges can cancel on a link.
      advi = advi + max(0.0_dp, lateral_correction)

      if (q1d2 < 0.0_dp) then
         qv = q1d2 / max(1.0e-5_dp, volu1d)
         u_mom = alpha_mom2 * q1d2 / au1d2
         u_ene = alpha_ene2 * q1d2 / au1d2
         advel = advel + am * (u_mom - u1) * qv + (1.0_dp - am) * (u_ene - u1) * qv
      end if
   end subroutine advect_pure1d_sobek

end module m_pure1d_advection
