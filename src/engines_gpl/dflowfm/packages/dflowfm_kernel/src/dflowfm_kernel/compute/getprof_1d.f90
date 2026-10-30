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

!
!
module m_get_prof_1D
   use m_rectan2d, only: rectan2d
   use m_rectan, only: rectan
   use m_yzprofile, only: yzprofile
   use m_pipe

   implicit none
   private :: store_hydraulic_radii
contains
   subroutine getprof_1D(L, hprL, area, width, japerim, calcConv, perim)
      use precision, only: dp
      use m_flow, only: bndwidth1d, lnxbnd, dmiss, u1, q1, s1, frcu, frcu_mor, u_to_umain, q1_main, cfuhi, ag, hu, ifrcutp, v, frcuni1dgrounlay
      use m_flow, only: q1_floodplain1, q1_floodplain2
      use m_flow, only: au_main, au_floodplain1, au_floodplain2, map_write_settings
      use m_flow, only: czu_main, czu_floodplain1, czu_floodplain2
      use m_flow, only: flow_width, flow_width_main, flow_width_floodplain1, flow_width_floodplain2
      use m_flowgeom, only: lnxi, lbnd1d, kcu, ln, wu, acl, prof1d, profiles1d, jagrounlay, grounlay, argr, pergr, wigr
      use unstruc_channel_flow, only: network, getcsparstotal, cscalculationoption, getcsparsflow
      use m_cross_helper, only: getconveyance, getcrossdischarge
      use m_flowtimes, only: time1, times_update_roughness
      use m_get_chezy, only: get_chezy
      use m_roughness, only: getchezy
      use network_data, only: LINK_1D_BOUNDARY

      integer :: L, japerim, calcConv
      real(kind=dp) :: hprL !< hoogte in profiel
      real(kind=dp) :: area !< wet cross sectional area
      real(kind=dp) :: width !< width at water surface
      real(kind=dp) :: perim !< wet perimeter

      real(kind=dp) :: profw !< width  of profile
      real(kind=dp) :: profh !< height of profile
      real(kind=dp) :: hydrad !< hydraulic radius

      real(kind=dp) :: area2, width2, perim2, cf2, alfa ! second prof i.c. interpolation
      real(kind=dp) :: alfg, czg, hpr

      real(kind=dp) :: frcn, cz, cf, conv, af_sub(3), perim_sub(3), cz_sub(3)
      real(kind=dp) :: q_sub(3) ! discharge per segment
      real(kind=dp), dimension(3) :: width_sub
      integer :: LL, ka, kb, itp, itpa, friction_type, ibndsect
      integer :: k1, k2
      integer :: jacustombnd1d
      real(kind=dp) :: u1L, q1L, s1L, dpt, factor, maxflowwidth

      LL = L
      if (L > lnxi) then ! for 1D boundary links, refer to attached link
         LL = LBND1D(L)
      end if

      hpr = hprL

      jacustombnd1d = 0
      if (kcu(L) == LINK_1D_BOUNDARY .and. allocated(bndWidth1D)) then
         ibndsect = lnxbnd(L - lnxi)
         if (ibndsect > 0) then
            if (bndWidth1D(ibndsect) /= dmiss) then
               jacustombnd1d = 1
            end if
         end if
      end if

      if (jacustombnd1d == 1) then ! This link is a 1D bnd *and* has a custom width.
         width = bndwidth1D(ibndsect)
         area = hpr * width
         perim = width + 2 * hpr

         if (japerim == 1 .and. calcConv == 1 .and. map_write_settings%au_split == 1) then
            au_main(L) = area
            au_floodplain1(L) = 0.0_dp
            au_floodplain2(L) = 0.0_dp
         end if

         ! Use the water level at the inner point of the boundary link
         k2 = ln(2, L)

         if ((japerim == 1) .and. (calcConv == 1)) then
            if (map_write_settings%flow_width == 1) then
               flow_width(L) = width
               flow_width_main(L) = width
               flow_width_floodplain1(L) = 0.0_dp
               flow_width_floodplain2(L) = 0.0_dp
            end if
            hydrad = area / perim
            perim_sub = [perim, 0.0_dp, 0.0_dp]
            af_sub = [area, 0.0_dp, 0.0_dp]
            if (map_write_settings%hydraulic_radius == 1) then
               call store_hydraulic_radii(L, [area, af_sub], [perim, perim_sub])
            end if
            !
            ! Calculate the conveyance and Chezy value, using the friction parameters on the internal link, using the
            ! local water depth on this boundary link.
            ! NOTE: In case of a YZ-type cross section the conveyance is computed, using the given water depth, but
            !       using the cross sectional profile of this YZ-cross section. In that case we need the Chezy value
            !       for computing the conveyance based on the rectangular.
            if (network%rgs%timeseries_defined) then
               factor = max(0.0_dp, (time1 - times_update_roughness(1)) / (times_update_roughness(2) - times_update_roughness(1)))
            else
               factor = 1.0_dp
            end if
            call getconveyance(network, hpr, u1(L), q1(L), s1(k2), LL, perim_sub, af_sub, conv, cz_sub, cz, area, perim, factor)
            if (map_write_settings%chezy_links_split == 1) then
               czu_main(L) = cz
               czu_floodplain1(L) = 0.0_dp
               czu_floodplain2(L) = 0.0_dp
            end if
            frcu(L) = cz
            frcu_mor(L) = cz
            u_to_umain(L) = 1.0_dp
            q1_main(L) = q1(L)
            q1_floodplain1(L) = 0.0_dp
            q1_floodplain2(L) = 0.0_dp
            wu(L) = width

            if (hydrad > 0.0_dp .and. cz > 0.0_dp) then
               cfuhi(L) = ag / (hydrad * cz * cz)
            else
               cfuhi(L) = 0.0_dp
            end if
         end if

         return

      else if ((abs(kcu(ll)) == 1 .or. abs(kcu(ll)) == 5) .and. (network%loaded .or. has_flow1d_cross_section(LL))) then !flow1d used only for 1d channels and not for 1d2d roofs and gullies
         cz = 0.0_dp

         if (japerim == 0) then ! calculate total area and volume
            call GetCSParsTotal(network%adm%line2cross(LL, 2), network%crs%cross, hpr, area, width, CSCalculationOption, network%adm%hysteresis_for_summerdike(:, LL))
         else ! japerim = 1: calculate flow area, conveyance and perimeter.
            cz = 0.0_dp
            call GetCSParsFlow(network%adm%line2cross(LL, 2), network%crs%cross, hpr, area, perim, width, &
                               maxflowwidth=maxflowwidth, af_sub=af_sub, perim_sub=perim_sub, width_sub=width_sub)

            if (calcConv == 1) then
               if (map_write_settings%flow_width == 1) then
                  flow_width(L) = width
                  flow_width_main(L) = width_sub(1)
                  flow_width_floodplain1(L) = width_sub(2)
                  flow_width_floodplain2(L) = width_sub(3)
               end if
               if (map_write_settings%hydraulic_radius == 1) then
                  call store_hydraulic_radii(L, [area, af_sub], [perim, perim_sub])
               end if
               if (map_write_settings%au_split == 1) then
                  au_main(L) = af_sub(1)
                  au_floodplain1(L) = af_sub(2)
                  au_floodplain2(L) = af_sub(3)
               end if
               q1_floodplain1(L) = 0.0_dp
               q1_floodplain2(L) = 0.0_dp
               u1L = u1(LL)
               q1L = q1(LL)
               dpt = hu(L)
               cz = 0.0_dp
               cz_sub = 0.0_dp
               if (network%rgs%timeseries_defined) then
                  factor = max(0.0_dp, (time1 - times_update_roughness(1)) / (times_update_roughness(2) - times_update_roughness(1)))
               else
                  factor = 1.0_dp
               end if
               if (abs(kcu(ll)) == 1) then
                  k1 = ln(1, LL)
                  k2 = ln(2, LL)
                  s1L = acl(L) * s1(k1) + (1.0_dp - acl(L)) * s1(k2)
                  call getconveyance(network, dpt, u1L, q1L, s1L, LL, perim_sub, af_sub, conv, cz_sub, cz, area, perim, factor)
               else ! 1D2Dlink
                  frcn = network%crs%cross(network%adm%line2cross(LL, 2)%c1)%frictionvaluepos(1) !>
                  friction_type = network%crs%cross(network%adm%line2cross(LL, 2)%c1)%frictiontypepos(1)
                  cz = getchezy(friction_type, frcn, area / perim, dpt, u1(LL))
                  cz_sub(1) = cz
                  conv = cz_sub(1) * af_sub(1) * sqrt(af_sub(1) / perim_sub(1))
               end if


               ! For sediment transport the discharge in the main channel is required:
               ! Qmain/ QT = Kmain/KT -> u_main = Kmain/KT * (AT/Amain)
               if (conv > 0.0_dp) then
                  u_to_umain(L) = area * cz_sub(1) * sqrt(af_sub(1) / perim_sub(1)) / conv
                  cfuhi(L) = ag / (conv / area)**2
                  frcu(L) = cz
                  frcu_mor(L) = cz_sub(1)
                  call getCrossDischarge(perim_sub, af_sub, cz_sub, q1(L), q_sub)
                  q1_main(L) = q_sub(1)
                  q1_floodplain1(L) = q_sub(2)
                  q1_floodplain2(L) = q_sub(3)

                  if (map_write_settings%chezy_links_split == 1) then
                     ! getconveyance has already converted subsection roughness to Chezy.
                     ! These coefficients combine through conveyance, not by summation.
                     ! For network 1D links ifrcutp is R_CHEZY, so get_chezy returns frcu unchanged.
                     ! czu_main(L) = get_chezy(hu(LL), cz(1), u1(LL), v(LL), ifrcutp(LL))
                     ! czu_floodplain1(L) = get_chezy(hu(LL), cz(2), u1(LL), v(LL), ifrcutp(LL))
                     ! czu_floodplain2(L) = get_chezy(hu(LL), cz(3), u1(LL), v(LL), ifrcutp(LL))
                  end if

               else
                  u_to_umain(L) = 1.0_dp
                  cfuhi(L) = 0.0_dp
               end if
            end if

            wu(L) = max(0.01_dp, maxflowwidth)

         end if
         ! finished for 1d network from flow1d
         return
      end if

! No flow1d cross input Proceed with conventional prof1D approach.
      if (prof1D(1, LL) >= 0) then ! direct profile based upon link value
         ka = 0
         kb = 0 ! do not use profiles
         profw = prof1D(1, LL)
         profh = prof1D(2, LL)
         itp = prof1D(3, LL)
         if (japerim == 1) then
            frcn = frcu(LL)
            friction_type = ifrcutp(LL)
         end if
      else
         ka = -prof1D(1, LL)
         kb = -prof1D(2, LL)
         profw = profiles1D(ka)%width
         profh = profiles1D(ka)%height
         itp = profiles1D(ka)%ityp
         alfa = prof1d(3, LL)
         itpa = itp

         if (japerim == 1) then
            if (profiles1D(ka)%frccf /= dmiss .and. profiles1D(kb)%frccf /= dmiss .and. &
                profiles1D(ka)%frctp == profiles1D(kb)%frctp) then
               frcn = (1.0_dp - alfa) * profiles1D(ka)%frccf + alfa * profiles1D(kb)%frccf
               friction_type = profiles1D(ka)%frctp
            else
               frcn = frcu(LL)
               friction_type = ifrcutp(LL)
            end if
         end if

      end if

      if (jagrounlay > 0) then
         hpr = hpr + grounlay(LL)
      end if

! base nrs == open, negative = closed
      if (abs(itp) == 1) then ! pipe
         call pipe(hpr, profw, area, width, japerim, perim)
      else if (abs(itp) == 2) then ! rectan, peri=wu + 2*hpr
         call rectan(hpr, profw, profh, area, width, japerim, perim, itp < 0)
      else if (abs(itp) == 3) then ! rectan, peri=wu
         call rectan2D(hpr, profw, profh, area, width, japerim, perim)
      else if (abs(itp) == 100 .or. abs(itp) == 101) then !                          itp >= 100, yzprof
         call yzprofile(hpr, ka, itp, area, width, japerim, frcn, friction_type, perim, cf)
      end if

      if (ka /= 0 .and. kb /= ka) then ! interpolate in profiles
         profw = profiles1D(kb)%width
         profh = profiles1D(kb)%height
         itp = profiles1D(kb)%ityp

         if (abs(itpa) < 100 .and. abs(itp) > 99) then
            ! doe hier backup cf
            if (frcn > 0) then
               hydrad = area / perim ! hydraulic radius
               cz = get_chezy(hydrad, frcn, u1(L), v(L), friction_type)
               cf = ag / (hydrad * cz * cz) ! see note on 2D conveyance in sysdoc5
            else
               cf = 0.0_dp
            end if
         end if

         if (abs(itp) == 1) then ! pipe
            call pipe(hpr, profw, area2, width2, japerim, perim2)
         else if (abs(itp) == 2) then ! rectan, peri=wu + 2*hpr
            call rectan(hpr, profw, profh, area2, width2, japerim, perim2, itp < 0)
         else if (abs(itp) == 3) then ! rectan, peri=wu
            call rectan2D(hpr, profw, profh, area2, width2, japerim, perim2)
         else if (abs(itp) == 100 .or. abs(itp) == 101) then ! >= 10, conveyance approach
            call yzprofile(hpr, kb, itp, area2, width2, japerim, frcn, friction_type, perim2, cf2)
         end if
         area = (1.0_dp - alfa) * area + alfa * area2
         width = (1.0_dp - alfa) * width + alfa * width2

         if (japerim == 1) then
            if (abs(itp) == 101) then ! 1D conveyance
               cf = (1.0_dp - alfa) * cf + alfa * cf2
            else
               perim = (1.0_dp - alfa) * perim + alfa * perim2
            end if
         end if
      end if

      if (jagrounlay > 0) then
         if (grounlay(LL) > 0) then
            area = area - argr(LL)
            if (japerim == 1) then
               perim = perim - pergr(LL) + wigr(LL)
            end if
         end if
      end if

      if (japerim == 1 .and. calcconv == 1) then

         if (map_write_settings%flow_width == 1) then
            flow_width(L) = width
            flow_width_main(L) = width
            flow_width_floodplain1(L) = 0.0_dp
            flow_width_floodplain2(L) = 0.0_dp
         end if

         if (map_write_settings%hydraulic_radius == 1) then
            if (abs(itp) == 101) then
               ! Legacy conveyance profiles do not return a wetted perimeter.
               call store_hydraulic_radii(L, [area, area, 0.0_dp, 0.0_dp], [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp])
            else
               call store_hydraulic_radii(L, [area, area, 0.0_dp, 0.0_dp], [perim, perim, 0.0_dp, 0.0_dp])
            end if
         end if

         if (map_write_settings%au_split == 1) then
            au_main(L) = area
            au_floodplain1(L) = 0.0_dp
            au_floodplain2(L) = 0.0_dp
         end if

         if (abs(itp) == 101) then ! 1D conveyance

            cfuhi(L) = cf

         else

            if (frcn > 0) then
               hydrad = area / perim ! hydraulic radius
               cz = get_chezy(hydrad, frcn, u1(L), v(L), friction_type)
               cfuhi(L) = ag / (hydrad * cz * cz) ! see note on 2D conveyance in sysdoc5
            else
               cfuhi(L) = 0.0_dp
            end if

            if (jagrounlay > 0) then
               if (grounlay(LL) > 0) then
                  czg = get_chezy(hydrad, frcuni1Dgrounlay, u1(L), v(L), friction_type)
                  alfg = wigr(LL) / perim
                  cfuhi(L) = (ag / hydrad) * (alfg / czg**2 + (1.0_dp - alfg) / cz**2)
               end if
            end if

         end if

         if (map_write_settings%chezy_links_split == 1) then
            czu_floodplain1(L) = 0.0_dp
            czu_floodplain2(L) = 0.0_dp
            ! Without subsections, use the same conversion as the link Chezy map output.
            czu_main(L) = 0.0_dp
            if (frcu(L) > 0.0_dp) then
               czu_main(L) = get_chezy(hu(L), frcu(L), u1(L), v(L), ifrcutp(L))
            end if
         end if

      end if

   end subroutine getprof_1D

   !> Store total and subsection hydraulic radii; dry sections are zero and missing wet perimeters give dmiss.
   subroutine store_hydraulic_radii(L, areas, perimeters)
      use precision, only: dp
      use m_missing, only: dmiss
      use m_flow, only: hydraulic_radius, hydraulic_radius_main, hydraulic_radius_floodplain1, hydraulic_radius_floodplain2

      integer, intent(in) :: L !< Flow link index.
      real(kind=dp), dimension(4), intent(in) :: areas !< Total, main channel and two floodplain flow areas.
      real(kind=dp), dimension(4), intent(in) :: perimeters !< Corresponding wet perimeters.
      real(kind=dp), dimension(4) :: radii

      radii = 0.0_dp
      where (areas > 0.0_dp .and. perimeters <= 0.0_dp)
         radii = dmiss
      end where
      where (areas > 0.0_dp .and. perimeters > 0.0_dp)
         radii = areas / perimeters
      end where

      hydraulic_radius(L) = radii(1)
      hydraulic_radius_main(L) = radii(2)
      hydraulic_radius_floodplain1(L) = radii(3)
      hydraulic_radius_floodplain2(L) = radii(4)
   end subroutine store_hydraulic_radii

   !> Returns true if the given link has a flow1d cross section, so that the 1D flow solver may be used even if no network is loaded.
   ! For now, the only links that can have this property are long culvert links.
   function has_flow1d_cross_section(L) result(res)
      use unstruc_channel_flow, only: network
      use m_longculverts_data, only: newculverts
      integer, intent(in) :: L
      logical :: res

      res = .false.
      ! For now this check is only valid for long culverts. Once
      if (associated(network%adm%line2cross) .and. newculverts) then
         if (L > 0 .and. L <= size(network%adm%line2cross, 1)) then
            res = (network%adm%line2cross(L, 2)%c1 > 0)
         end if
      end if

   end function has_flow1d_cross_section

end module m_get_prof_1D
