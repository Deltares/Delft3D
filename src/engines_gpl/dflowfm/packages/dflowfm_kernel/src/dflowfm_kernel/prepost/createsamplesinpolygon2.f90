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

   subroutine CREATESAMPLESINPOLYGON2()
      use m_ec_triangle
      use network_data, only: TRIANGLESIZEFAC
      !use m_netw
      use M_SAMPLES
      use M_MISSING
      use m_sferic
      use m_alloc
      use geometry_module, only: dbpinpol, get_startend
      use m_polygon

      implicit none

      !integer          :: NPL
      !double precision :: XPL(NPL), YPL(NPL)

      integer :: ierr
      integer :: in
      integer :: n
      integer :: nn
      integer :: ns1, NPL1
      integer :: ntx, I

      double precision :: TRIAREA, SAFESIZE
      double precision :: AREPOL, DLENPOL, DLENAV, DLENMX, XP, YP, xplmin, xplmax, yplmin, yplmax

      if (NPL <= 2) return

      call DAREAN(XPL, YPL, NPL, AREPOL, DLENPOL, DLENMX)

      DLENAV = DLENPOL / NPL ! AVERAGE SIZE ON POLBND
!   TRIAREA  = 0.5d0*DLENAV*DLENAV  ! AVERAGE TRIANGLE SIZE
      TRIAREA = 0.25d0 * sqrt(3d0) * DLENAV * DLENAV ! AVERAGE TRIANGLE SIZE

      SAFESIZE = 11 ! SAFETY FACTOR

      if (jsferic == 1) then
         ! DLENPOL and AREPOL are in metres, whereas Triangle gets spherical
         ! coordinates, so first scale desired TRIAREA back to spherical.
         xplmin = 0d0; xplmax = dlenpol / 4d0; yplmin = 0d0; yplmax = dlenpol / 4d0
         call get_startend(NPL, XPL, YPL, n, nn, dmiss)
         if (nn > n) then
            xplmin = minval(xpl(n:nn))
            xplmax = maxval(xpl(n:nn))
            yplmin = minval(ypl(n:nn))
            yplmax = maxval(ypl(n:nn))
         end if
         triarea = triarea * (xplmax - xplmin) * (yplmax - yplmin) / arepol
         NTX = SAFESIZE * (xplmax - xplmin) * (yplmax - yplmin) / triarea
      else
         NTX = SAFESIZE * AREPOL / TRIAREA
      end if

      if (NTX < 2) then
         call QNERROR('TRISIZE MAYBE TOO LARGE FOR POLYGON?', ' ', ' ')
      end if

      !NTX = 10

!  start pointer
      NS1 = NS + 1

      numtri = -1
      NN = -1
      do while (numtri < 0 .or. NN < 0)
         if (allocated(INDX)) then
            deallocate (INDX)
         end if
         allocate (INDX(3, NTX), STAT=IERR); INDX = 0
         call AERR('INDX(3,NTX)', IERR, int(3 * NTX))

         call realloc(EDGEINDX, (/2, Ntx/), keepExisting=.false., fill=0, stat=ierr)
         call realloc(TRIEDGE, (/3, Ntx/), keepExisting=.false., fill=0, stat=ierr)

         NN = NTX
         call increasesam(NS1 + NN)
         zs(ns1:ubound(zs, 1)) = 0d0 ! zkuni ! SPvdP: used to be DMISS, but then the samples are not plotted

         TRIAREA = TRIANGLESIZEFAC * TRIANGLESIZEFAC * TRIAREA
         NPL1 = NPL
         do I = 1, NPL
            if (xpl(I) == dmiss) then
               NPL1 = I - 1
               exit
            end if
         end do

         numtri = ntx ! Input value should specify max nr of triangles in indx.
         NN = ntx ! used to check array size of xs, ys in tricall
         call TRICALL(2, XPL, YPL, NPL1, INDX, NUMTRI, EDGEINDX, NUMEDGE, TRIEDGE, XS(NS1), YS(NS1), NN, TRIAREA)
         if (numtri < 0) ntx = -numtri
         if (nn < 0) ntx = max(ntx, -nn)
      end do

      IN = -1 ! EN BIJPLUGGEN
      do N = NS1, NS1 + NN
         XP = XS(N); YP = YS(N)
         call DBPINPOL(XP, YP, IN, dmiss, JINS, NPL, xpl, ypl, ypl)
         if (IN == 1) then
            NS = NS + 1
            XS(NS) = XP; YS(NS) = YP
         end if
      end do

      return
   end subroutine CREATESAMPLESINPOLYGON2
