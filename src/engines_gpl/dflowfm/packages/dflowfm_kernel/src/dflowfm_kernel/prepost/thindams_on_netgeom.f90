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

!> Put the polyline thin dams on the network links.
!! All crossed net links are set to kn(3,L) = 0, such that flow_geominit
!! does not even create a flow link across it.
subroutine thindams_on_netgeom()
   use m_crspath_on_netgeom, only: crspath_on_netgeom
   use m_thindams
   use network_data
   use unstruc_messages
   use m_alloc
   use kdtree2Factory
   use m_wall_clock_time
   use m_delpol
   use m_find_crossed_links_kdtree2
   use m_get_link_neighboring_cell_coords
   use m_append_crspath_to_pol
   implicit none

   double precision, dimension(:), allocatable :: dSL
   integer, dimension(:), allocatable :: iLink, ipol, idum

   double precision :: xza, yza, xzb, yzb

   double precision :: t0, t1

   character(len=128) :: mesg

   integer :: ierror ! error (1) or not (0)

   integer :: numcrossedLinks

   integer :: isactive

   integer :: ic, iL, L, LL, NPL_prev

   integer :: jakdtree = 1 ! use kdtree (1) or not (0)

   if (nthd == 0) return

   ierror = 1

   if (jakdtree == 1) then
      call wall_clock_time(t0)

!         determine set of links that are connected by a path
      allocate (iLink(numL))
      allocate (iPol(numL))
      allocate (dSL(numL))
      allocate (idum(3 * nthd))

      call delpol()

!         copy all paths to a DMISS-separated polyline
      do ic = 1, nthd
         NPL_prev = NPL ! previous end pointer in polyline array
         call appendCRSPathToPol(thd(ic))
         if (NPL > 0) then
            if (NPL > ubound(idum, 1)) then
               call realloc(idum, 1 + int(1.2d0 * dble(NPL)), keepExisting=.true., fill=0)
            end if
            idum(NPL_prev + 1:NPL) = ic
         end if
      end do

      call find_crossed_links_kdtree2(treeglob, NPL, xpl, ypl, 1, numL, 0, numcrossedlinks, iLink, iPol, dSL, ierror)
      if (ierror /= 0) then
         !          disable kdtree
         jakdtree = 0

         !          deallocate
         if (allocated(iLink)) deallocate (iLink)
         if (allocated(ipol)) deallocate (ipol)
         if (allocated(dSL)) deallocate (dSL)
         if (allocated(idum)) deallocate (idum)
      else
!            initialize number of crossed flowlinks in paths
         do ic = 1, nthd
            thd(ic)%lnx = 0
         end do

         do iL = 1, numcrossedlinks
!               get link number
            L = iLink(iL)
!               get thin dam number
            ic = idum(iPol(iL))
            call get_link_neighboringcellcoords(L, isactive, xza, yza, xzb, yzb)
            if (isactive == 1) then
               call crspath_on_singlelink(thd(ic), L, xk(kn(1, L)), yk(kn(1, L)), xk(kn(2, L)), yk(kn(2, L)), xza, yza, xzb, yzb, 1)
               do L = 1, thd(ic)%lnx
                  LL = abs(thd(ic)%ln(L))
                  if (LL > 0 .and. LL <= numl) then
                     kn(3, LL) = 0
                  end if
               end do
            end if
         end do ! do iL=1,numcrossedlinks
      end if

      call wall_clock_time(t1)
      write (mesg, "('thin dams with kdtree2, elapsed time: ', G15.5, 's.')") t1 - t0
      call mess(LEVEL_INFO, trim(mesg))
   end if

   if (jakdtree == 0) then ! no kdtree, or kdtree gave error
      call wall_clock_time(t0)
      do ic = 1, nthd
         call crspath_on_netgeom(thd(ic))
         do L = 1, thd(ic)%lnx
            LL = abs(thd(ic)%ln(L))
            if (LL > 0 .and. LL <= numl) then
               kn(3, LL) = 0
            end if
         end do
      end do
      call wall_clock_time(t1)
      write (mesg, "('thin dams without kdtree2, elapsed time: ', G15.5)") t1 - t0
      call mess(LEVEL_INFO, trim(mesg))
   end if ! if ( jakdtree.eq.1 ) then

   ierror = 0
1234 continue

   if (allocated(iLink)) deallocate (iLink)
   if (allocated(iPol)) deallocate (iPol)
   if (allocated(dSL)) deallocate (dSL)
   if (allocated(idum)) deallocate (idum)

   if (NPL > 0) call delpol()

   return

end subroutine thindams_on_netgeom
