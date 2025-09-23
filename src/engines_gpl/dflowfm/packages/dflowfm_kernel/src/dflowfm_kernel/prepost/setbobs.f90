!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
module m_set_bobs
   use m_get2dnormal, only: get2dnormal
   use m_get1ddir, only: get1ddir
   use m_duikerstoprofs

   implicit none
contains
   subroutine setbobs() ! and set blu, weigthed depth at u point
      use precision, only: dp
      use m_netw, only: netcell, zk, zkuni, numl, numl1d, lne, kn, nmk
      use m_flowparameters, only: ibedlevtyp, calc_bedlevel_over_inactive_links, jaconveyance2D
      use m_flowparameters, only: BEDLEV_TYPE_WATERLEVEL, BEDLEV_TYPE_VELOCITY, BEDLEV_TYPE_MEAN, BEDLEV_TYPE_MIN, BEDLEV_TYPE_MAX, BEDLEV_TYPE_WATERLEVEL6
      use m_flowgeom, only: ndx2d, bl, ndxi, lne2ln, iadv, ln, lncn, ibot, blu, bob, bob0, lnx1d, lnx, kcu, kcs, ndx, nd
      use m_flowgeom, only: wu, lnxi
      use m_flow, only: ibedlevmode, BLMODE_D3D, BLMODE_DFM, dmiss, jaupdbobbl1d, setHorizontalBobsFor1d2d, jaupdbndbl, jadpuopt
      use m_flow, only: blmeanbelow, blminabove
      use m_sediment, only: stm_included
      use m_oned_functions, only: setbobs_1d
      use m_structures, only: network
      use m_longculverts

      integer :: L, k1, k2, n1, n2, n, k, k3, LL, kk, Ls, Lf, mis, i, numcoords, ibotL
      real(kind=dp) :: bl1, bl2, bedlevel_at_link, bln, zn1, zn2, zn3, wn, alf, skewn, xt, yt, xn, yn
      ! real(kind=dp), external :: skewav

      ! First, prepare bed levels at pressure points:

      if (ibedlevmode == BLMODE_D3D) then
         ! DPSOPT=MAX equivalent: deepest zk/corner point
         do k = 1, ndx2d ! TODO: [TRUNKMERGE] WO: I restored ndx2d (was: ndx1db in sedmor)
            bl(k) = huge(1.0_dp)
            do kk = 1, netcell(k)%n
               zn1 = zk(netcell(k)%nod(kk)); if (zn1 == dmiss) zn1 = zkuni
               bl(k) = min(bl(k), zn1)
            end do
         end do
      else
         ! Default: BLMODE_DFM, tiles or velocity point based, use ibedlevtyp only
         if (ibedlevtyp == BEDLEV_TYPE_WATERLEVEL) then ! Already delivered via ext file, only fill missing values here
            do k = 1, ndxi
               if (bl(k) == dmiss) then
                  bl(k) = zkuni
               end if
            end do
         else if (ibedlevtyp > BEDLEV_TYPE_WATERLEVEL .and. ibedlevtyp <= BEDLEV_TYPE_MAX) then
            bl = 1.0e30_dp
         else if (ibedlevtyp == BEDLEV_TYPE_WATERLEVEL6) then ! quick and dirty flownodes tile depth like taken from netnodes, to be able to at least run netnode zk defined models
            do k = 1, ndxi ! Was: ndx2d, but netcell includes 1D too
               bl(k) = 0.0_dp
               mis = 0
               do kk = 1, netcell(k)%n
                  bl(k) = bl(k) + zk(netcell(k)%nod(kk))
                  if (zk(netcell(k)%nod(kk)) == dmiss) mis = 1
               end do
               if (mis == 1) then
                  bl(k) = zkuni
               else
                  bl(k) = bl(k) / netcell(k)%n
               end if
            end do

         end if
      end if

      do L = numL1D + 1, numL ! Intentional: includes boundaries, to properly set bobs based on net nodes here already
         Lf = lne2ln(L)

         bedlevel_at_link = huge(1.0_dp)
         ! prevent out of bounds, when bedlevel_at_link is not set:
         n1 = 1
         n2 = 1

         ! When Lf (flow link) <= 0, there is no active flow link at this interface
         ! Bedlevtyp == 2 uses only the pre-defined bed levels in BLU
         if (Lf <= 0 .and. ibedlevtyp /= BEDLEV_TYPE_VELOCITY .and. calc_bedlevel_over_inactive_links) then
            n1 = lne(1, L)
            n2 = lne(2, L)
            if (n1 == 0 .and. n2 == 0) then
               cycle
            else if (n1 == 0) then
               n1 = n2
            else if (n2 == 0) then
               n2 = n1
            end if
            k1 = kn(1, L)
            k2 = kn(2, L)
            bedlevel_at_link = get_bedlevel_at_link(n1, n2, k1, k2, dmiss, 0)

         else if (Lf > 0) then
            if (iadv(Lf) > 20 .and. iadv(Lf) < 30) cycle ! skip update of bobs for structures ! TODO: [TRUNKMERGE]: JN/BJ: really structures on bnd?

            n1 = ln(1, Lf)
            n2 = ln(2, Lf)
            k1 = lncn(1, Lf)
            k2 = lncn(2, Lf)
            if (allocated(ibot)) then
               ibotL = ibot(Lf)
            else
               ibotL = 0
            end if
            bedlevel_at_link = get_bedlevel_at_link(n1, n2, k1, k2, blu(Lf), ibotL)
            if (jaconveyance2D >= 1) then
               if (zk(k1) == dmiss) then
                  bob(1, Lf) = zkuni
               else
                  bob(1, Lf) = zk(k1)
               end if
               if (zk(k2) == dmiss) then
                  bob(2, Lf) = zkuni
               else
                  bob(2, Lf) = zk(k2)
               end if

            else
               bob(1, Lf) = bedlevel_at_link
               bob(2, Lf) = bedlevel_at_link
            end if
            blu(Lf) = min(bob(1, Lf), bob(2, Lf))
         end if

         if (ibedlevmode == BLMODE_DFM .and. ibedlevtyp /= BEDLEV_TYPE_WATERLEVEL .and. ibedlevtyp /= BEDLEV_TYPE_WATERLEVEL6) then
            bl(n1) = min(bl(n1), bedlevel_at_link)
            bl(n2) = min(bl(n2), bedlevel_at_link)
         end if

      end do
      bob0(:, lnx1d + 1:lnx) = bob(:, lnx1d + 1:lnx)

      if (jaupdbobbl1d > 0) then
         call setbobs_1d()
         jaupdbobbl1d = 0 ! update bobs and bl only at initialization. After initialisation bobs should only follow from bl, in particular for morphological updating. When considering nodal relations, some special treatment may be required
      else

         do L = 1, lnx1D ! 1D

            if (iadv(L) > 20 .and. iadv(L) < 30 .and. (.not. stm_included)) cycle ! skip update of bobs for structures

            n1 = ln(1, L); n2 = ln(2, L) ! flow ref
            k1 = lncn(1, L); k2 = lncn(2, L) ! net  ref
            zn1 = zk(k1); if (zn1 == dmiss) zn1 = zkuni
            zn2 = zk(k2); if (zn2 == dmiss) zn2 = zkuni

            if (kcu(L) == 1) then ! 1D link

               if (ibedlevtyp == BEDLEV_TYPE_WATERLEVEL .or. ibedlevtyp == BEDLEV_TYPE_WATERLEVEL6) then ! tegeldieptes celcentra ! TODO: [TRUNKMERGE] WO/BJ: do we need stm_included in this if (consistent?)
                  if (stm_included) then
                     bl1 = bl(n1)
                     bl2 = bl(n2)
                     bob(1, L) = max(bl1, bl2)
                     bob(2, L) = bob(1, L)
                  else ! Old non-MOR code for 1D in models with tiledepths
                     bob(1, L) = zn1
                     bob(2, L) = zn2
                     bl(n1) = zn1
                     bl(n2) = zn2
                  end if
               else
                  bedlevel_at_link = 0.5_dp * (zn1 + zn2) ! same as 2D, based on network, but now in flow link dir. In 2D this is net link dir
                  bob(1, L) = bedlevel_at_link
                  bob(2, L) = bedlevel_at_link ! revisit
                  bl(n1) = min(bl(n1), bedlevel_at_link)
                  bl(n2) = min(bl(n2), bedlevel_at_link)
               end if
            end if
         end do
         bob0(:, 1:lnx1d) = bob(:, 1:lnx1d)
      end if

      ! 1d-2d links
      do L = 1, lnx1D ! 1D
         n1 = ln(1, L); n2 = ln(2, L) ! flow ref
         k1 = lncn(1, L); k2 = lncn(2, L) ! net  ref
         if (ibedlevtyp == BEDLEV_TYPE_MEAN) then
            zn1 = zk(k1)
            zn2 = zk(k2)
         else if (ibedlevtyp == BEDLEV_TYPE_WATERLEVEL .or. ibedlevtyp == BEDLEV_TYPE_WATERLEVEL6) then
            zn1 = bl(n1)
            zn2 = bl(n2)
         else
            zn1 = blu(L)
            zn2 = blu(L)
         end if
         if (zn1 == dmiss) zn1 = zkuni
         if (zn2 == dmiss) zn2 = zkuni

         if (kcu(L) == 3) then ! 1D2D internal link, bobs at minimum
            if (kcs(n1) == 21) then
               bedlevel_at_link = bl(n1)
               call get2Dnormal(n1, xn, yn) ! xn, yn = 2D land normal vector pointing upward, both zero = flat
               call get1Ddir(n2, xt, yt) ! xt, yt = 1D river tangential normal vector
            end if
            if (kcs(n2) == 21) then
               bedlevel_at_link = bl(n2)
               call get2Dnormal(n2, xn, yn)
               call get1Ddir(n1, xt, yt)
            end if
            skewn = abs(xn * xt + yn * yt)
            bob(1, L) = bedlevel_at_link
            bob(2, L) = bedlevel_at_link ! revisit later+ wu(L)*skewn ! TODO: HK: why wu here? Why not dx(L) or something similar?
            bob0(1, L) = bedlevel_at_link
            bob0(2, L) = bedlevel_at_link ! revisit later+ wu(L)*skewn ! TODO: HK: why wu here? Why not dx(L) or something similar?
            bl(n1) = min(bl(n1), bedlevel_at_link)
            bl(n2) = min(bl(n2), bedlevel_at_link)
         else if (kcu(L) == BEDLEV_TYPE_MIN) then ! left right
            bedlevel_at_link = min(zn1, zn2)
            bob(1, L) = zn1
            bob(2, L) = zn2
            bob0(1, L) = zn1
            bob0(2, L) = zn2
            bl(n1) = min(bl(n1), bedlevel_at_link)
            bl(n2) = min(bl(n2), bedlevel_at_link)
         else if (kcu(L) == 5 .or. kcu(L) == 7) then ! keep 1D and 2D levels
            if (bl(n1) /= 1.0e30_dp) then
               bob(1, L) = bl(n1)
            else
               bob(1, L) = zn1
            end if
            if (bl(n2) /= 1.0e30_dp) then
               bob(2, L) = bl(n2)
            else
               bob(2, L) = zn2
            end if
            if (zk(k1) /= dmiss .and. nmk(k1) == 1) then ! if zk specified at endpoint
               bob(1, L) = zk(k1)
            end if
            if (zk(k2) /= dmiss .and. nmk(k2) == 1) then ! if zk specified at endpoint
               bob(2, L) = zk(k2)
            end if
            if (setHorizontalBobsFor1d2d) then
               bob(:, L) = max(bob(1, L), bob(2, L))
            end if
            bob0(:, L) = bob(:, L)
            bl(n1) = min(bl(n1), bob(1, L))
            bl(n2) = min(bl(n2), bob(2, L))
         end if
      end do

      do k = 1, ndx !losse punten die geen waarde kregen
         if (bl(k) == 1.0e30_dp) then
            bl(k) = zkuni
         end if
      end do

      do L = lnxi + 1, lnx ! randjes copieren

         if (iadv(L) > 20 .and. iadv(L) < 30) cycle ! skip update of bobs for structures

         n1 = ln(1, L); n2 = ln(2, L)
         if (jaupdbndbl == 1) then
            !if `jadpuopt==1`, the bed level at the boundaries has been extrapolated in `setbedlevelfromextfile` and we do not want to overwrite it.
            if (jadpuopt == 1) then
               bl(n1) = bl(n2) !original
            end if
         end if

         if (kcu(L) == -1) then ! 1D randjes extrapoleren voor 1D straight channel convecyance testcase
            k1 = lncn(1, L); k2 = lncn(2, L)
            k3 = 0
            do k = 1, nd(n2)%lnx
               LL = abs(nd(n2)%ln(k))
               if (kcu(LL) == 1) then
                  if (nd(n2)%ln(k) < 0) k3 = lncn(2, LL)
                  if (nd(n2)%ln(k) > 0) k3 = lncn(1, LL)
               end if
            end do

            if (ibedlevtyp == BEDLEV_TYPE_WATERLEVEL .or. ibedlevtyp == BEDLEV_TYPE_WATERLEVEL6) then
               bl2 = bl(n2)
               if (stm_included) then
                  bl1 = bl(n1)
               else
                  bl1 = bl2
               end if
               bl(n1) = bl1
               bob(1, L) = max(bl(n1), bl(n2))
               bob(2, L) = bob(1, L)
               !elseif (bl(n1) == 1d30 .or. bl(n2) == 30) then
            else if (.not. network%loaded) then
!          SPvdP: previous expression is problematic when zk(k2) and/or zk(k3) have missing values
               zn2 = zk(k2); if (zn2 == dmiss) zn2 = zkuni
               zn3 = zk(k3); if (zn3 == dmiss) zn3 = zkuni
               zn1 = 1.5_dp * zn2 - 0.5_dp * zn3 ! note: actual locations of cells centers not taken into account

               bob(1, L) = zn1
               bob(2, L) = zn1
               bob0(:, L) = zn1
               bl(n1) = min(bl(n1), zn1)
               bl(n2) = min(bl(n2), zn1)
            end if

         else ! 2D boundary link
            if (ibedlevtyp == BEDLEV_TYPE_WATERLEVEL .or. ibedlevtyp == BEDLEV_TYPE_WATERLEVEL6) then ! Implicitly intended for: jaconveyance2D < 1
               bob(1, L) = bl(n1) ! uniform bobs only for tiledepths
               bob(2, L) = bl(n1)
               if (stm_included) then
                  if (jadpuopt == 1) then
                     bob(1, L) = max(bl(n1), bl(n2))
                  elseif (jadpuopt == 2) then
                     bob(1, L) = (bl(n1) + bl(n2)) / 2
                  end if
                  bob(2, L) = bob(1, L)
                  bob0(:, L) = bob(1, L)
               end if
            end if
         end if
      end do

      call duikerstoprofs()

      if (newculverts) then
         ! find the 1d2d flowlinks required for longculvertsToProfs
         do i = 1, nlongculverts
            numcoords = size(longculverts(i)%xcoords)
            call find1d2dculvertlinks(network, longculverts(i), numcoords)
            !this routine is called here because the culvert links need to be filled, cannot be done during Geominit.
            call setLongCulvert1D2DLinkAngles(i)
         end do
         call longculvertsToProfs(.true.)
      else
         call longculvertsToProfs(.false.)
      end if
      if (blmeanbelow /= -999.0_dp) then
         do n = 1, ndx2D
            wn = 0.0_dp; bln = 0.0_dp
            do LL = 1, nd(n)%lnx
               Ls = nd(n)%ln(LL); L = abs(Ls)
               bln = bln + wu(L) * 0.5_dp * (bob(1, L) + bob(2, L))
               wn = wn + wu(L)
            end do
            if (wn > 0.0_dp) then
               bln = bln / wn
               alf = min(1.0_dp, (blminabove - bln) / (blminabove - blmeanbelow))
               if (alf > 0.0_dp) then
                  bl(n) = alf * bln + (1.0_dp - alf) * bl(n)
               end if
            end if
         end do
      end if
      jaupdbndbl = 0 ! after first run of setbobs set to 0 = no update

   end subroutine setbobs
   !> calculate bed level at a link based on the type of bed level definition and the input parameters.
   function get_bedlevel_at_link(n1, n2, k1, k2, blu, ibot) result(bedlevel_at_link)

      use precision, only: dp
      use m_missing, only: dmiss
      use network_data, only: zkuni, zk
      use m_flowparameters, only: ibedlevtyp, jadpuopt, jaconveyance2D
      use m_flowparameters, only: BEDLEV_TYPE_WATERLEVEL, BEDLEV_TYPE_VELOCITY, BEDLEV_TYPE_MEAN, BEDLEV_TYPE_MIN, BEDLEV_TYPE_MAX, BEDLEV_TYPE_WATERLEVEL6
      use m_flowgeom, only: bl

      integer, intent(in) :: n1, n2 !< Node numbers for the link.
      integer, intent(in) :: k1, k2 !< Corresponding net node numbers (corner points).
      integer, intent(in) :: ibot !< Location dependent bedlevel type (overrides ibedlevtyp).
      real(kind=dp), intent(in) :: blu !< Bed level at u point, used for ibedlevtyp=2.

      real(kind=dp) :: bedlevel_at_link

      real(kind=dp) :: zn1, zn2

      if (ibedlevtyp == BEDLEV_TYPE_WATERLEVEL .or. ibedlevtyp == BEDLEV_TYPE_WATERLEVEL6) then ! tile bed levels at cell centers
         if (jadpuopt == 1) then !original
            bedlevel_at_link = max(bl(n1), bl(n2))
         elseif (jadpuopt == 2) then
            bedlevel_at_link = (bl(n1) + bl(n2)) / 2
         end if

      else if (ibedlevtyp == BEDLEV_TYPE_VELOCITY) then ! directly apply bed levels at flow links,
         if (blu == dmiss) then
            bedlevel_at_link = zkuni
         else
            bedlevel_at_link = blu
         end if
      else if (ibedlevtyp >= BEDLEV_TYPE_MEAN .or. ibedlevtyp <= BEDLEV_TYPE_MAX) then ! bed levels from netnodes zk
         zn1 = zk(k1)
         if (zn1 == dmiss) then
            zn1 = zkuni
         end if

         zn2 = zk(k2)
         if (zn2 == dmiss) then
            zn2 = zkuni
         end if

         if (jaconveyance2D >= 1) then ! left right
            bedlevel_at_link = min(zn1, zn2)
         else if (ibedlevtyp == BEDLEV_TYPE_MEAN) then ! mean
            bedlevel_at_link = 0.5_dp * (zn1 + zn2)
         else if (ibedlevtyp == BEDLEV_TYPE_MIN) then ! min
            bedlevel_at_link = min(zn1, zn2)
         else if (ibedlevtyp == BEDLEV_TYPE_MAX) then ! max
            bedlevel_at_link = max(zn1, zn2)
         end if

         if (ibot == BEDLEV_TYPE_MIN) then
            bedlevel_at_link = min(zn1, zn2) ! local override min
         else if (ibot == BEDLEV_TYPE_MAX) then ! local override max
            bedlevel_at_link = max(zn1, zn2)
         end if
      end if
   end function get_bedlevel_at_link
end module m_set_bobs
