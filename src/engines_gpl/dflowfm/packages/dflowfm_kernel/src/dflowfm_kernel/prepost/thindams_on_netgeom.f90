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
module m_thindams_on_netgeom

   implicit none

   private

   public :: thindams_on_netgeom

contains

   subroutine thindams_on_netgeom()
      use precision, only: dp
      use m_crspath_on_netgeom, only: crspath_on_netgeom
      use m_thindams, only: thd, nthd, crspath_on_singlelink
      use network_data, only: numl, kn, npl, xpl, ypl, xk, yk, kn
      use messagehandling, only: LEVEL_INFO, mess
      use m_alloc, only: realloc
      use kdtree2Factory, only: treeglob
      use m_wall_clock_time, only: wall_clock_time
      use m_delpol, only: delpol
      use m_find_crossed_links_kdtree2, only: find_crossed_links_kdtree2
      use m_get_link_neighboring_cell_coords, only: get_link_neighboringcellcoords
      use m_append_crspath_to_pol, only: appendCRSPathToPol
      use unstruc_caching, only: cache_retrieved, cache_thin_dams, copy_cached_thin_dams
      use precision, only: dp

      real(kind=dp), dimension(:), allocatable :: dSL
      integer, dimension(:), allocatable :: iLink, ipol, idum
      real(kind=dp) :: xza, yza, xzb, yzb
      real(kind=dp) :: t0, t1
      character(len=128) :: mesg
      integer :: ierror ! error (1) or not (0)
      integer :: numcrossedLinks
      integer :: isactive
      integer :: ic, iL, L, LL, NPL_prev
      integer :: jakdtree ! use kdtree (1) or not (0)
      logical :: cache_read

      if (nthd == 0) return

      ierror = 1
      jakdtree = 1

      cache_read = .false.
      if (cache_retrieved()) then
         call copy_cached_thin_dams(thd, cache_read)
      end if

      if (cache_read) then
         do ic = 1, nthd
            do L = 1, thd(ic)%lnx
               LL = abs(thd(ic)%ln(L))
               if (LL > 0 .and. LL <= numl) then
                  kn(3, LL) = 0
               end if
            end do
         end do
      else
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
         end if ! if (jakdtree == 1) then

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
         end if ! if (jakdtree == 0) then

         ierror = 0

         if (NPL > 0) call delpol()

         call cache_thin_dams(thd)

      end if ! if (.not. cache_read) then

   end subroutine thindams_on_netgeom

end module m_thindams_on_netgeom
