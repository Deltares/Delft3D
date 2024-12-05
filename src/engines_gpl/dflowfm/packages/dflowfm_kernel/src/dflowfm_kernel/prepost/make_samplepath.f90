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

! make a heighest walk in a structured sample set
subroutine make_samplepath(xp, yp)
   use precision, only: dp
   use m_netw
   use m_samples
   use m_arcinfo
   use m_alloc
   use m_missing

   implicit none

   real(kind=dp), intent(inout) :: xp, yp !< coordinates of start point

   integer, dimension(:), allocatable :: ipsub

   integer :: ierror, ip, ip0, ip1
   integer :: ipnext, ipcur, ipprev
   integer :: Nsub, isub
   integer :: iter, idir, ipol1, ipol2

   integer, parameter :: MAXITER = 10000

   if (MXSAM * MYSAM /= NS) goto 1234 ! no structured sample data

!  find first sample point
   call ispoi1(xs, ys, NS, xp, yp, ip0)
   if (ip0 < 1 .or. ip0 > NS) return

!  allocate
   Nsub = 1
   allocate (ipsub(Nsub))

   ipol1 = 0
   ipol2 = 0

   ipcur = ip0
   ipnext = ipcur
   do idir = 1, 2
      do iter = 1, MAXITER
         ipprev = ipcur
         ipcur = ipnext
         do
            Nsub = ubound(ipsub, 1)
            call makestep_samplepath(ipprev, ipcur, ipnext, Nsub, ipsub, ierror)
            if (ierror < 0) then
               call realloc(ipsub, Nsub)
            else
               exit
            end if
         end do

!        remember first step
         if (iter == 1) ip1 = ipnext

         if (ipnext < 1 .or. ipnext == ipcur) exit

!        add trajectory to polygon
         if (iter == 1) then
!           create new polyline
            if (NPL > 0) then
               if (xpl(NPL) /= DMISS) then
                  call increasepol(NPL + 3, 1)
                  xpl(NPL + 1) = DMISS
                  ypl(NPL + 1) = DMISS
                  zpl(NPL + 1) = DMISS
                  NPL = NPL + 1
               else
                  call increasepol(NPL + 2, 1)
               end if
            else
               call increasepol(NPL + 2, 1)
            end if
!           add first point
            xpl(NPL + 1) = xs(ipcur)
            ypl(NPL + 1) = ys(ipcur)
            zpl(NPL + 1) = zs(ipcur)
            NPL = NPL + 1

!           remember index of first point in polygon
            if (idir == 1) then
               ipol1 = NPL
            else
               ipol2 = NPL
            end if
         else
            !     add to polyline
            call increasepol(NPL + 1, 1)
         end if

         !  add new point
         xpl(NPL + 1) = xs(ipnext)
         ypl(NPL + 1) = ys(ipnext)
         zpl(NPL + 1) = zs(ipnext)
         NPL = NPL + 1

!        disable samples in the subpath, except the next sample, and the current in the first pass
         do isub = 1, Nsub
            ip = ipsub(isub)
            if (ip /= ipnext .and. (ip /= ip0 .or. idir /= 1)) then
               zs(ip) = DMISS
            end if
         end do
      end do ! do iter=1,MAXITER

!     next path: reverse first step
      ipcur = ip1
      ipnext = ip0
   end do ! do idir=1,2

!  merge the two polylines
   if (ipol1 > 0 .and. ipol2 > 0) then
      call mergepoly(xpl, ypl, zpl, MAXPOL, NPL, ipol1, ipol2)
   end if

   ierror = 0
1234 continue

! deallocate
   if (allocated(ipsub)) deallocate (ipsub)

   return
end subroutine make_samplepath
