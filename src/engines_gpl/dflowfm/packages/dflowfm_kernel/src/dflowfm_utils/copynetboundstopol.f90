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

!> copy the network boundaries to polygon
!! The polygon will be placed in global m_polygon::XPL, etc.
!! Multiple polygons ('segments') will be separated with a single DMISS in the arrays.
!! For each polygon, the first two points will contain in ZPL the original net node numbers.
!! The makecounterclockwise option will orient the polygon CCW if it encloses the grid,
!! and CW if it represents a hole in the grid.
!! NOTE: when using the makecounterclockwise=1 option, the two net node numbers in ZPL may
!! have been flipped to the last two indices for the polygons that were re-oriented.
subroutine copynetboundstopol(inpol, needfindcells, makecounterclockwise, setnetstat)

   use m_alloc
   use m_polygon
   use m_missing
   use network_data
   use m_sferic, only: jsferic
   use geometry_module, only: dbpinpol, pinpok, cross, get_startend
   use gridoperations

   implicit none

   integer, intent(in) :: inpol !< net boundaries in polygon only (1) or not (0)
   integer, intent(in) :: needfindcells !< call findcells (1) or not (0)
   integer, intent(in) :: makecounterclockwise !< Return outer polygons in counterclockwise orientation (1) or not (0) (default: 0). NOTE: for 'holes' in the grid, the polygon will be made clockwise.
   integer, intent(in) :: setnetstat !< set netstat (1), may induce findcells, or not (0)

   integer :: L, LI, LL, kstart, kcur, kp, kr, maxpolh, nph0, nphstart, &
              inhul1, inhul2, jasecondtail, iseg, iseg0, isegc, nseg, iorient, &
              JACROS, jacros1, jacros2, &
              i1, i2, i3, ia, ib, ic, idir, ia0, ib0, ic0, idir0, &
              npn, inland, ifindtailcross, jstart, jend, &
              jinside
   integer, allocatable :: jalinkvisited(:)
   integer, allocatable :: isegstart(:)
   double precision :: xkb, ykb, zkb, SL, SL0, sl1, sl2, SM, XCR, YCR, CRP, xcg, ycg
   double precision, allocatable :: xpn(:), ypn(:), zpn(:)

   if (numL < 1) return ! nothing to do

   allocate (jalinkvisited(numl))
   allocate (isegstart(numl)) ! (much less than numl needed, generally)
   allocate (xpn(numl), ypn(numl), zpn(numL))
   npn = 0
   inland = 1 ! Require from user first poly point is on land.
   idir = 0 ! SPvdP: initialization
   jalinkvisited = 0 ! SPvdP: initialization

   if (inpol == 0) then
      call savepol()
      npl = 0
      xpl = dmiss
      ypl = dmiss
      zpl = dmiss
      if (needfindcells == 1) call findcells(0)
      call restorepol()
   else
      if (needfindcells == 1) call findcells(0)
   end if

   inhul1 = -1; inhul2 = -1
! Construct the new polygon set in XPH (backup pol is not used anway during this operation)
   XPH = dmiss
   NPH = 0
   maxpolh = size(xph)
   nseg = 0
   do L = 1, numl
      if (jalinkvisited(L) == 1) cycle
      if (kn(3, L) /= 2 .and. kn(3, L) /= 0) cycle ! No 1D nor 1D2D
      if (lnn(L) /= 1) cycle
      if (kn(1, L) < 1 .or. kn(2, L) < 1) cycle ! safety
      call dbpinpol(XK(kn(1, L)), YK(kn(1, L)), inhul1, dmiss, JINS, NPL, xpl, ypl, zpl)
      call dbpinpol(XK(kn(2, L)), YK(kn(2, L)), inhul2, dmiss, JINS, NPL, xpl, ypl, zpl)
      if (inhul1 /= 1 .and. inhul2 /= 1) cycle

      if (NPH + 3 > maxpolh) then
         maxpolh = max(NPH + 3, ceiling(maxpolh * 1.2))
         call realloc(xph, maxpolh)
         call realloc(yph, maxpolh)
         call realloc(zph, maxpolh)
      end if

      ! Start a new polyline
      if (NPH > 0) then ! Separate from existing polylines
         NPH = NPH + 1
         XPH(NPH) = dmiss
         YPH(NPH) = dmiss
         ZPH(NPH) = dmiss
      end if

      ! This a new link, so start walking its first tail
      jasecondtail = 0

      ! start point
      NPH = NPH + 1
      nseg = nseg + 1
      isegstart(nseg) = nph
      nphstart = nph
      kstart = kn(1, L)
      XPH(NPH) = XK(kstart)
      YPH(NPH) = YK(kstart)
      ZPH(NPH) = dble(kstart)
!    CALL CIRR(XK(kstart), YK(kstart), 71)
      ! Add second point and then...
      kcur = kn(2, L)
      NPH = NPH + 1
      XPH(NPH) = XK(kcur)
      YPH(NPH) = YK(kcur)
      ZPH(NPH) = dble(kcur)
!    CALL CIRR(XK(kcur), YK(kcur), 81)
      jalinkvisited(L) = 1
      ! ... start walking connected netlinks
10    continue
      ! If current point is not within user-polygon, then finish this segment directly.
      ! This way, for a crossing netlink both netnodes are put in xph, but no further than that.
      call dbpinpol(XK(kcur), YK(kcur), inhul2, dmiss, JINS, NPL, xpl, ypl, zpl)
      if (inhul2 /= 1) then
         goto 11
      end if
      do LI = 1, NMK(kcur)
         LL = NOD(kcur)%lin(LI)
         if (jalinkvisited(LL) == 1) cycle
         if (LNN(LL) == 1) then
            if (kn(2, LL) == kcur) then
               kcur = kn(1, LL)
            else
               kcur = kn(2, LL)
            end if
            NPH = NPH + 1
            if (NPH > maxpolh) then
               maxpolh = ceiling(maxpolh * 1.2)
               call realloc(xph, maxpolh)
               call realloc(yph, maxpolh)
               call realloc(zph, maxpolh)
            end if
            XPH(NPH) = XK(kcur)
            YPH(NPH) = YK(kcur)
            ZPH(NPH) = dble(kcur)
            jalinkvisited(LL) = 1
!            CALL CIRR(XK(kcur), YK(kcur), 31)
            goto 10
         end if
      end do
11    continue
      if (kcur == kstart) then ! polyline closed itself
         !...
      else if (jasecondtail /= 1) then
         ! Now grow a polyline starting at the other side of the original link L, i.e., the second tail
         kcur = kstart
         nph0 = nph
         jasecondtail = 1
         goto 10
      else
         ! Completed a second tail for netlink L, concat with previous
         if (nph > nph0) then
            ! There *is* a nonempty second tail, so reverse the first tail, so that they connect.
            do KP = nphstart + ceiling((nph0 - nphstart + 1) / 2.), nph0
               xkb = xph(kp)
               ykb = yph(kp)
               zkb = zph(kp)
               kr = nph0 - KP + nphstart
               xph(kp) = xph(kr)
               yph(kp) = yph(kr)
               zph(kp) = zph(kr)
               xph(kr) = xkb
               yph(kr) = ykb
               zph(kr) = zkb
            end do
         end if
      end if
      ! Finished current link L (either closed or two tails), proceed with next L
   end do
   isegstart(nseg + 1) = nph + 2
! Now check for all begin and end segments of the new polygon whether they
! intersect with user-polygon. If so,

!call polorientation(xpl, ypl, npl, iorient)
!
   inland = 1 ! Assume (i.e., require from user) that first poly point lies on land.
   ifindtailcross = 0
   do i1 = 1, npl
      i2 = mod(i1, npl) + 1
      if (ifindtailcross == 1) then
         call CROSS(xpl(i1), ypl(i1), xpl(i2), ypl(i2), xpn(npn - 1), ypn(npn - 1), xpn(npn), ypn(npn), JACROS, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
         if (jacros == 1) then
            npn = npn - 1 ! Remove last netbd point (was outside user poly, temp. needed for this cross check)
            npn = npn + 1 !eigenlijk hier ook multiple isects checken? of nee
            xpn(npn) = xpl(i2)
            ypn(npn) = ypl(i2)
            zpn(npn) = zpl(i2)
!            CALL CIRR(xpn(npn), ypn(npn), 61)
            ifindtailcross = 0
            inland = abs(inland - 1)
         end if
         cycle
      end if

      ia0 = -999 ! Reset 'previous crossing' indicator
      do iseg = 1, nseg
         isegc = iseg
         ia = abs(isegstart(iseg))
         ib = ia + 1
         crp = 0d0
         call CROSS(xpl(i1), ypl(i1), xpl(i2), ypl(i2), xph(ia), yph(ia), xph(ib), yph(ib), JACROS1, SL1, SM, XCR, YCR, CRP, jsferic, dmiss)

         ia = abs(isegstart(iseg + 1)) - 2
         ib = ia - 1
         call CROSS(xpl(i1), ypl(i1), xpl(i2), ypl(i2), xph(ia), yph(ia), xph(ib), yph(ib), JACROS2, SL2, SM, XCR, YCR, CRP, jsferic, dmiss)
         if (jacros1 == 1 .and. (jacros2 == 0 .or. jacros2 == 1 .and. sl1 < sl2)) then
            ia = abs(isegstart(iseg))
            ib = ia + 1
            ic = abs(isegstart(iseg + 1)) - 2 ! End of segment
            idir = 1 ! Walk segment in forward direction
            sl = sl1
         elseif (jacros2 == 1 .and. (jacros1 == 0 .or. jacros1 == 1 .and. sl2 < sl1)) then
            ic = abs(isegstart(iseg)) ! 'End' of segment
            idir = -1 ! Walk segment in reverse direction
            sl = sl2
         end if
         if (jacros1 == 1 .and. jacros2 == 1) then
            ! both head and tail of iseg cross with a single poly segment, include it directly.
            goto 20
         end if
         jacros = max(jacros1, jacros2)

         if (jacros == 1) then
            inland = abs(inland - 1)
            if (ia0 == -999 .and. iseg < nseg) then
               ia0 = ia; ib0 = ib; ic0 = ic; idir0 = idir; sl0 = sl
               iseg0 = iseg
               cycle ! Allow a second crossing
            else
               if (ia0 /= -999 .and. sl0 > sl) then
                  ia = ia0; ib = ib0; ic = ic0; idir = idir0; sl = sl0
                  isegc = iseg0
               end if
            end if
         else ! jacross == 0
            if (iseg == nseg) then
               if (ia0 /= -999) then
                  ia = ia0; ib = ib0; ic = ic0; idir = idir0; sl = sl0
                  isegc = iseg0
                  jacros = 1
               end if
            else
               cycle ! No crossing with segment iseg, try next one
            end if
         end if

20       if (inland == 0) then ! on water, so include this user-poly point in new poly
            npn = npn + 1
            xpn(npn) = xpl(i2)
            ypn(npn) = ypl(i2)
            zpn(npn) = zpl(i2)
!            CALL CIRR(xpn(npn), ypn(npn), 61)
         elseif (isegstart(isegc) > 0) then
            ! isegstart(.) > 0 when not yet crossed this segment
            ! so include it in the growing new polygon (possibly reversed)
            ! do not include first point (it's outside of user polygon)
            ! do include last point (for checking tail crossing, will be removed later)
            if (idir > 0) then ! SPvdP: check added, this gave problems in snap-to-land with polygon
               do i3 = ia + idir, ic, idir
                  npn = npn + 1
                  xpn(npn) = xph(i3)
                  ypn(npn) = yph(i3)
                  zpn(npn) = zph(i3)
                  !            CALL CIRR(xpn(npn), ypn(npn), 61)
                  ifindtailcross = 1
                  isegstart(isegc) = -isegstart(isegc)
               end do
            end if
         end if
         exit ! If we got this far, no further segments need to be checked in iseg loop
      end do
   end do

   if (ifindtailcross == 1) then
      npn = npn - 1
   end if

   do iseg = 1, nseg
      if (isegstart(iseg) > 0) then
         npn = npn + 1
         xpn(npn) = dmiss
         ypn(npn) = dmiss
         zpn(npn) = dmiss

         ia = abs(isegstart(iseg))
         ic = abs(isegstart(iseg + 1)) - 2 ! End of segment
         idir = 1

         if (makecounterclockwise == 1) then
            ! Detect whether this polygon encloses the grid, or is a hole in the grid.
            call findel(int(zph(ia)), int(zph(ia + 1)), L) ! zph still contains the first two net node numbers where this pol started.
            if (L /= 0) then ! safety, should always hold
               if (LNN(L) == 1) then ! safety, should always hold
                  ! Use center of mass of this first neighbouring grid cell, for "inside/outside" check.
                  xcg = xzw(lne(1, L))
                  ycg = yzw(lne(1, L))
                  call pinpok(xcg, ycg, ic - ia + 1, xph(ia:ic), yph(ia:ic), jinside, jins, dmiss) ! is pol enclosing the grid or a hole in the grid.

                  call polorientation(xph(ia:ic), yph(ia:ic), ic - ia + 1, ic - ia + 1, iorient) ! current pol may be CCW or CW

                  ! Check polygon type: outer ring enclosing the grid, should be counterclockwise. A hole in the grid should be clockwise.
                  if ((jinside == 1 .and. iorient == -1) .or. (jinside == 0 .and. iorient == 1)) then
                     ! Polygon needs to be reversed, simply copy points in reverse order:
                     ib = ia
                     ia = ic
                     ic = ib
                     idir = -1
                  else
                     ! Polygon already ok.
                     idir = 1
                  end if
               end if
            end if
         end if

         do i2 = ia, ic, idir
            npn = npn + 1
            xpn(npn) = xph(i2)
            ypn(npn) = yph(i2)
            zpn(npn) = zph(i2)
         end do
         isegstart(iseg) = -isegstart(iseg)
      end if
   end do

   call savepol() ! put user-poly in undo buffer

! remove leading DMISS values from polygon
   call get_startend(npn, xpn, ypn, jstart, jend, dmiss)
   npl = npn - (jstart - 1)
   call increasepol(npl, 0)
   do i1 = 1, npl
      xpl(i1) = xpn(i1 - 1 + jstart)
      ypl(i1) = ypn(i1 - 1 + jstart)
      zpl(i1) = zpn(i1 - 1 + jstart)
   end do
!zpl = dmiss ! AvD: TODO: netbound zk values in zpl
! SPvdP: stored pointer to netnode in zk

   deallocate (jalinkvisited, isegstart, xpn, ypn, zpn)

   if (setnetstat == 1) then
!  polygon changed: set netstat to dirty
      netstat = NETSTAT_CELLS_DIRTY
   end if

end subroutine copynetboundstopol
