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

   subroutine teknetcells(netwhat, jahalt, jacol)
      use m_netw
      use m_flowgeom
      use unstruc_display
      use m_missing
      use m_partitioninfo
      use m_alloc
      use unstruc_model, only: md_netfile
      use unstruc_messages
      use m_sferic, only: jsferic, dg2rd
      use gridoperations
      use m_vfac
      use m_drawthis
      use m_halt2

      implicit none

      integer, intent(in) :: netwhat, jahalt, jacol
      double precision :: xx(6), yy(6), aspect, uu1, vv1, uu2, vv2
      double precision :: xfac
      integer :: k, kk, ja, ncol, nodemode, nn
      integer :: ntopology, numcellstoplot
      double precision, external :: znetcell
      double precision, external :: coarsening_info
      logical inview

      if (netwhat <= 1) return

      nodemode = NDRAW(19)

      ntopology = numk + numl
      if (ntopology /= lasttopology .and. lasttopology > 0) then ! coarsening info
         if (ubound(lnn, 1) /= numL) then
            !        if (md_genpolygon == 0 .and. len_trim(md_netfile)>0) then
            !           if (netwhat.ge.15.and.netwhat.le.19 ) then
            !              jaidomain = 1
            !           else
            !              jaidomain = 0
            !           end if
            !           call preparecells(md_netfile, jaidomain, ierr)
            !           if (ierr .ne. 0) then
            !              call mess(LEVEL_WARN, 'Cannot find cell info. and will enforce findcells')
            !           endif
            !        else
            !            ierr = -1 ! Force findcells hereafter.
            !        end if
            !        if (ierr /= 0) then
            call findcells(100)
            if (netwhat == 2 .or. netwhat >= 15 .and. netwhat <= 19) then
               call find1dcells() ! partitioning info
            end if
            if (netwhat >= 15 .and. netwhat <= 19) then
               call mess(LEVEL_WARN, 'Cannot find partition info.')
               NDRAW(33) = 0
            end if
!         endif
         else
            call setnodadm(0) ! in case the administration is out of date
         end if
         call makenetnodescoding()
      end if

      numcellstoplot = nump
      if (netwhat == 2 .or. netwhat >= 15 .and. netwhat <= 19) numcellstoplot = nump1d2d ! only for cell or domain numbers

      if (numcellstoplot > size(rlin)) then
         call realloc(rlin, numcellstoplot)
      end if

      call setcol(0)

!  uncomment the following to refresh netcell administration, based on module variable netstat
!   if ( netstat /= NETSTAT_OK ) then
!      call findcells(100)
!   end if

!  scalars
      if (netwhat /= 4) then
         if (netwhat == 7) then ! coarsening info
            do k = 1, nump
               rlin(k) = coarsening_info(k)
            end do
         else if (netwhat < 14) then ! default
            do k = 1, numcellstoplot
               rlin(k) = znetcell(k)
            end do
         else if ((netwhat == 15 .or. netwhat == 16) .and. allocated(idomain)) then ! partitioning info
            if (size(idomain) >= numcellstoplot) then
               if (netwhat == 15) then
                  do k = 1, numcellstoplot
                     rlin(k) = dble(idomain(k))
                  end do
               else if (netwhat == 16 .and. allocated(numndx)) then ! partitioning info
                  do k = 1, numcellstoplot
                     rlin(k) = dble(numndx(idomain(k)))
                  end do
               end if
            end if
         else if (netwhat == 17 .or. netwhat == 18 .or. netwhat == 19) then ! ghost levels
            if (allocated(ighostlev)) then
               if (size(ighostlev) >= numcellstoplot) then
                  do k = 1, numcellstoplot
                     if (netwhat == 17) then
                        rlin(k) = dble(ighostlev(k))
                     else if (netwhat == 18) then
                        rlin(k) = dble(ighostlev_cellbased(k))
                     else
                        rlin(k) = dble(ighostlev_nodebased(k))
                     end if
                  end do
               end if
            end if
         else if (netwhat == 20) then ! global cell number
            if (allocated(iglobal_s)) then
               if (size(iglobal_s) >= numcellstoplot) then
                  do k = 1, numcellstoplot
                     rlin(k) = dble(iglobal_s(k))
                  end do
               end if
            end if
         end if

         if (nodemode == 3 .or. nodemode == 6 .and. netwhat < 14) then
            call copynetcellstonetnodes()
         end if

         call MINMXNETCELLS()

         do k = 1, numcellstoplot
            if (mod(k, 200) == 0) then
               if (jahalt /= -1234) call halt2(ja)
               if (ja == 1) return
            end if

            if (inview(xzw(k), yzw(k)) .and. rlin(k) /= DMISS) then
               if (nodemode == 2 .or. nodemode == 6 .or. &
                   nodemode == 7 .or. nodemode == 8) then ! numbers
                  call setcol(1)
                  if (netwhat == 2 .or. netwhat == 15) then ! cell numbers or domain numbers
                     call dhitext(int(rlin(k)), xzw(k) + RCIR, yzw(k) - RCIR)
                  else
                     call dhtext(dble(rlin(k)), xzw(k) + RCIR, yzw(k) - RCIR, yzw(k))
                  end if
               end if
               if (nodemode == 3 .or. nodemode == 6) then ! isolines within cell
                  call ISOSMOOTHnet(k)
               else if (nodemode == 4 .or. nodemode == 7) then ! isofil= cellfill
                  call isocol(dble(rlin(k)), ncol)
                  nn = netcell(k)%n
                  do kk = 1, nn
                     xx(kk) = xk(netcell(k)%nod(kk))
                     yy(kk) = yk(netcell(k)%nod(kk))
                  end do
                  call PFILLER(xx, yy, nn, NCol, NCol)
               else if (nodemode == 5 .or. nodemode == 8) then
                  call isocol(dble(rlin(k)), ncol)
                  call drcirc(xz(k), yz(k), dble(rlin(k)))
               end if
            end if
         end do
      end if

!  vectors
      if (netwhat == 4 .or. netwhat == 5) then
         do k = 1, numcellstoplot
            if (mod(k, 200) == 0) then
               if (jahalt /= -1234) call halt2(ja)
               if (ja == 1) return
            end if

            if (inview(xzw(k), yzw(k))) then
               call orthonet_compute_orientation(aspect, uu1, vv1, uu2, vv2, k)
               if (jacol == 1) call setcol(3)

               if (jsferic == 1) then
                  xfac = 1d0 / cos(yzw(k) * dg2rd)
               else
                  xfac = 1d0
               end if

               if (uu1**2 + vv1**2 < uu2**2 + vv2**2) then
                  if (jacol == 1) call setcol(3)
                  call arrowsxy(xzw(k), yzw(k), uu1 * xfac, vv1, 0.5d0 * VFAC)
                  call arrowsxy(xzw(k), yzw(k), uu1 * xfac, vv1, -0.5d0 * VFAC)
                  if (jacol == 1) call setcol(221)
               else
                  if (jacol == 1) call setcol(221)
                  call arrowsxy(xzw(k), yzw(k), uu1 * xfac, vv1, 0.5d0 * VFAC)
                  call arrowsxy(xzw(k), yzw(k), uu1 * xfac, vv1, -0.5d0 * VFAC)
                  if (jacol == 1) call setcol(3)
               end if
               call arrowsxy(xzw(k), yzw(k), uu2 * xfac, vv2, 0.5d0 * VFAC)
               call arrowsxy(xzw(k), yzw(k), uu2 * xfac, vv2, -0.5d0 * VFAC)
            end if
         end do
      end if

   end subroutine TEKNETCELLS
