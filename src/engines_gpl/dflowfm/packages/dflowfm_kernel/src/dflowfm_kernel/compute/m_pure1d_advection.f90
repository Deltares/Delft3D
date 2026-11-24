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
!> One-dimensional momentum advection along 1D channels (jaPure1D > 0).
!!
!! Options 1 and 2 use a Perot-type representative node velocity (setuc1d_perot).
!! Options 3 to 5 use the SOBEK-type link advection of M. Borsboom, "Junction advection"
!! (2017-12-29, SVN 65523): 3 = momentum conserving, 4 = weighted momentum/energy conserving,
!! 5 = energy conserving. setuc1d prepares the node and link data once per time step;
!! advect_pure1d_sobek evaluates the advection term per link.
module m_pure1d_advection

   implicit none

   private

   integer, parameter, public :: pure1d_momentum_conserving = 3 !< jaPure1D option: momentum conserving SOBEK-type advection.
   integer, parameter, public :: pure1d_weighted_momentum_energy_conserving = 4 !< jaPure1D option: area-ratio weighted mix of momentum and energy conserving advection.
   integer, parameter, public :: pure1d_energy_conserving = 5 !< jaPure1D option: energy conserving SOBEK-type advection.

   public :: setuc1d
   public :: advect_pure1d_sobek

contains

   !> Set representative node velocities (uc1D) and upwind link velocities (u1Du) for jaPure1D options 1 and 2.
   !! The node velocity is the mean of the discharge-weighted inflow and outflow velocities, signed by the
   !! first connected link. Nodes with non-1D links, and junctions when jaJunction1D == 0, are skipped.
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
      real(kind=dp) :: flow_cs_width !< [m] surface width of flow area
      real(kind=dp) :: total_width !< [m] surface width of total (flow + storage) area
      real(kind=dp) :: dzw_dt !< [m/s] water level change rate
      real(kind=dp) :: lateral_flow !< [m3/s] lateral flow into the node

      if (kmx /= 0 .or. lnx1D == 0) then
         return
      end if

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

   !> Prepare the 1D advection data for the current time step.
   !! Does nothing for 3D models, models without 1D links, or jaPure1D == 0. Options 1 and 2 call
   !! setuc1d_perot; options 3 and higher call prepare_1d_advection.
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

   !> Prepare lateral corrections and junction fluxes for the SOBEK advection scheme (jaPure1D >= 3).
   !! Per 1D node, it fills the half-link geometry (au1D, sar1D, wu1D, volu1D), the reconstructed
   !! node-end discharges (q1D), and the junction factors alpha_mom_1D and alpha_ene_1D that scale
   !! the reconstructed velocities of outflowing links to balance momentum and energy fluxes.
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

         ! Sections 3.5.1-3.5.2 propose a small floor on the incoming flux
         ! balance; here a fixed 1e-20 floor is applied to outgoing sums.
         ! Eqs. (44)-(47): one factor per junction scales the reconstructed
         ! outgoing velocities. Momentum flux is linear in that factor and
         ! energy flux quadratic, hence the square root for alpha_ene_1D.
         alpha_mom_1D(n) = qu_in / max(1e-20_dp, qu_out)
         alpha_ene_1D(n) = sqrt(qu2_in / max(1e-20_dp, qu2_out))
      end do

   end subroutine prepare_1d_advection

   !> Update the half-link geometry of all links connected to node n, and sum the net discharge and
   !! surface area over those links. Fills wu1D, au1D and sar1D at the node end of each link, and adds
   !! the half-link flow volume to volu1D.
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

   !> Reconstruct the discharges at the node ends of the links connected to node n (q1D), and sum
   !! the incoming and outgoing momentum and energy fluxes at the node.
   !! The net inflow is distributed over the links in proportion to their half-link surface area,
   !! so that the reconstructed discharges satisfy mass conservation at the node.
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

   !> Compute the lateral and storage correction per link without modifying global state.
   !! Per 1D node, the lateral inflow minus the storage gain is shared equally over the connected links
   !! and divided by each link's flow volume au*dx. No correction is applied at time1 <= tstart_user.
   pure subroutine prepare_lateral_correction(ndx2D, ndxi, nd, qin, volume_change, vol1_f, vol0_f, &
                                              dts, time1, tstart_user, au, dx, eps3, lateral_correction)
      use m_flowgeom, only: tnode
      use precision, only: dp
      implicit none

      integer, intent(in) :: ndx2D !< Number of 2D flow nodes; 1D nodes start at ndx2D + 1.
      integer, intent(in) :: ndxi !< Number of internal flow nodes.
      type(tnode), dimension(:), intent(in) :: nd !< Flow node administration.
      real(kind=dp), dimension(:), intent(in) :: qin !< [m3/s] Lateral inflow per node.
      real(kind=dp), dimension(:), intent(in) :: volume_change !< [m3] Total volume change per node during the last time step.
      real(kind=dp), dimension(:), intent(in) :: vol1_f !< [m3] Flow volume per node at the end of the time step.
      real(kind=dp), dimension(:), intent(in) :: vol0_f !< [m3] Flow volume per node at the start of the time step.
      real(kind=dp), intent(in) :: dts !< [s] Time step.
      real(kind=dp), intent(in) :: time1 !< [s] Current time.
      real(kind=dp), intent(in) :: tstart_user !< [s] Start time of the simulation.
      real(kind=dp), dimension(:), intent(in) :: au !< [m2] Flow area per link.
      real(kind=dp), dimension(:), intent(in) :: dx !< [m] Link length.
      real(kind=dp), intent(in) :: eps3 !< [m2] Minimum flow area for a link to receive a correction.
      real(kind=dp), dimension(:), intent(out) :: lateral_correction !< [1/s] Lateral and storage correction per link.

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

               h = max(0.0_dp, s1(n) - bob(k, La)) ! cross sectional area
               call getprof_1D(La, h, total_cs_area, total_width, JACSTOT, CALCCONV, perim)
               call getprof_1D(La, h, flow_cs_area, flow_cs_width, JACSFLW, CALCCONV, perim)
               link_surface_area = total_width * half_link_length
               wu1D(k, La) = total_width
               au1D(k, La) = flow_cs_area
               sar1D(k, La) = link_surface_area

               surface_area = surface_area + link_surface_area
               volu1D(La) = volu1D(La) + flow_cs_area * half_link_length

               q_net_in = q_net_in + real(sign(1, L), kind=dp) * qa(La)
            end do
         end do
      end if

   end subroutine prepare_lateral_correction

   !> Compute the SOBEK-type 1D advection term for one link (jaPure1D >= 3).
   !! Only the upstream link end contributes. The momentum part transports the reconstructed upwind
   !! velocity with Q/V; the energy part is ((u^U)**2 - u**2)/(2 dx), cf. eq. (27) of the junction
   !! advection memo. The weight am selects momentum (1), energy (0), or the area-ratio mix.
   pure subroutine advect_pure1d_sobek(jaPure1D, u1, au1d1, au1d2, q1d1, q1d2, volu1d, dx, alpha_mom1, alpha_ene1, &
                                       alpha_mom2, alpha_ene2, lateral_correction, advi, advel)
      use precision, only: dp
      implicit none

      integer, intent(in) :: jaPure1D !< 1D advection option: 3 = momentum, 4 = weighted, 5 = energy conserving.
      real(kind=dp), intent(in) :: u1 !< [m/s] Link velocity.
      real(kind=dp), intent(in) :: au1d1 !< [m2] Flow area at the start of the link.
      real(kind=dp), intent(in) :: au1d2 !< [m2] Flow area at the end of the link.
      real(kind=dp), intent(in) :: q1d1 !< [m3/s] Reconstructed discharge at the start of the link.
      real(kind=dp), intent(in) :: q1d2 !< [m3/s] Reconstructed discharge at the end of the link.
      real(kind=dp), intent(in) :: volu1d !< [m3] Flow volume of the link.
      real(kind=dp), intent(in) :: dx !< [m] Link length.
      real(kind=dp), intent(in) :: alpha_mom1 !< [-] Momentum junction factor at the start node.
      real(kind=dp), intent(in) :: alpha_ene1 !< [-] Energy junction factor at the start node.
      real(kind=dp), intent(in) :: alpha_mom2 !< [-] Momentum junction factor at the end node.
      real(kind=dp), intent(in) :: alpha_ene2 !< [-] Energy junction factor at the end node.
      real(kind=dp), intent(in) :: lateral_correction !< [1/s] Lateral and storage correction of the link.
      real(kind=dp), intent(inout) :: advi !< [1/s] Implicit advection coefficient; the positive part of lateral_correction is added.
      real(kind=dp), intent(inout) :: advel !< [m/s2] Explicit advection term; overwritten.

      real(kind=dp) :: am
      real(kind=dp) :: qv
      real(kind=dp) :: u_ene
      real(kind=dp) :: u_mom

      advel = 0.0_dp
      ! The weighted option uses a 1e-4 lower bound on the area-ratio denominator.
      select case (jaPure1D)
      case (3) ! momentum conserving
         am = 1.0_dp
      case (4) ! weighted
         am = min(au1d1, au1d2) / max(1.0e-4_dp, au1d1, au1d2)
      case (5) ! energy conserving
         am = 0.0_dp
      end select

      if (q1d1 > 0.0_dp) then
         qv = q1d1 / max(1.0e-5_dp, volu1d)
         u_mom = alpha_mom1 * q1d1 / au1d1
         u_ene = alpha_ene1 * q1d1 / au1d1
         advel = advel - am * (u_mom - u1) * qv - (1.0_dp - am) * (u_ene**2 - u1**2) / (2.0_dp * dx)
      end if

      if (q1d2 < 0.0_dp) then
         qv = q1d2 / max(1.0e-5_dp, volu1d)
         u_mom = alpha_mom2 * q1d2 / au1d2
         u_ene = alpha_ene2 * q1d2 / au1d2
         advel = advel + am * (u_mom - u1) * qv + (1.0_dp - am) * (u_ene**2 - u1**2) / (2.0_dp * dx)
      end if

      advi = advi + max(0.0_dp, lateral_correction)
   end subroutine advect_pure1d_sobek

end module m_pure1d_advection
