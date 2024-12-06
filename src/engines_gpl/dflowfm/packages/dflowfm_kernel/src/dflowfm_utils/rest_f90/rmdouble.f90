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

      subroutine RMDOUBLE(XS, YS, ZS, IPSAM, NS)
         use precision, only: dp
         use m_missing
         use m_sferic
         use unstruc_messages
         use kdtree2Factory
         use m_wall_clock_time

         implicit none

         integer :: i
         integer :: j
         integer :: jadouble
         integer :: k
         integer :: ns
         integer :: nsorg
         integer :: numweg
         integer :: isam, jsam
         real(kind=dp), intent(inout) :: XS(NS), YS(NS), ZS(NS)
         integer, dimension(NS), intent(inout) :: IPSAM !< permutation array (increasing x-coordinate)
         integer, dimension(:), allocatable :: newnode

         real(kind=dp), dimension(:), allocatable :: xx, yy ! non-missing sample coordinates

         integer, dimension(:), allocatable :: iperm ! permutation array

         real(kind=dp) :: t0, t1, t2, t3, t4
         integer :: ii, jj, NN, num, nummerged, ierror
         integer :: jakdtree = 1

         integer :: jsferic_store

         character(len=128) :: txt

         real(kind=dp), parameter :: dtol2 = 1d-8 ! sample-on-top of each other tolerance, squared

         character OUD * 8
         NSORG = NS

         allocate (newnode(NS))

!     store jsferic
         jsferic_store = jsferic

!     safety
         !if ( NS.lt.100 ) then
         !   jakdtree=0
         !end if

         call wall_clock_time(t0)

         if (jakdtree == 1) then
!        force Cartesian coordinates
            jsferic = 0
!        get non-missing sample coordinates
            allocate (xx(NS))
            xx = 0d0
            allocate (yy(NS))
            yy = 0d0
            allocate (iperm(NS))
            iperm = 0

!        get non-missing sample coordinates
            num = 0
            do i = 1, NS
               if (xs(i) /= DMISS .and. ys(i) /= DMISS) then
                  num = num + 1
                  xx(num) = xs(i)
                  yy(num) = ys(i)
                  iperm(num) = i
               end if
            end do

!        initialize kdtree
            call build_kdtree(treeglob, num, xx, yy, ierror, jsferic, dmiss)

!        deallocate arrays with non-missing node coordinates
            deallocate (xx)
            deallocate (yy)

            if (ierror /= 0) then
!           deallocate permutation array
               if (allocated(iperm)) deallocate (iperm)

!           deallocate kdtree
               if (treeglob%itreestat /= ITREE_EMPTY) call delete_kdtree2(treeglob)

!           disable kdtree
               jakdtree = 0
            end if
         end if

         nummerged = 0

!     fill double samples with DMISS
5        continue
         JADOUBLE = 0

         if (jakdtree == 1) then
!        find samples on top of each other
            do ii = 1, num
               i = iperm(ii)

               if (i == 0) cycle ! already merged

!           fill query vector
               call make_queryvector_kdtree(treeglob, xs(i), ys(i), jsferic)

!           count number of points in search area
               NN = kdtree2_r_count(treeglob%tree, treeglob%qv, dtol2)

               if (NN > 1) then ! at least two samples need to be merged
!              resize results array if necessary
                  call realloc_results_kdtree(treeglob, NN)

!              find other nodes
                  call kdtree2_n_nearest(treeglob%tree, treeglob%qv, NN, treeglob%results)

!              merge with other nodes
                  do k = 1, NN
                     jj = treeglob%results(k)%idx
                     j = iperm(jj)
!                 exclude own sample and samples already deleted
                     if (j /= i .and. j > 0) then
                        if (xs(i) == xs(j) .and. ys(i) == ys(j)) then ! NOT SPHERICAL-PROOF
                           iperm(jj) = 0
                           xs(j) = DMISS
                           jadouble = 1
                           nummerged = nummerged + 1
                        end if
                     end if
                  end do
               end if
            end do
         else !  non kdtree
            do I = 1, NS - 1
               ISAM = IPSAM(I)
               if (XS(ISAM) /= XYMIS .and. ZS(ISAM) /= DMISS) then
                  J = I
15                continue
                  if (J < NS) then
                     J = J + 1
                     JSAM = IPSAM(J)
                     if (XS(ISAM) == XS(JSAM)) then
                        if (YS(ISAM) == YS(JSAM)) then
                           XS(JSAM) = XYMIS
                           JADOUBLE = 1
                        end if
                        goto 15
                     end if
                  end if
               end if
            end do

         end if

         call wall_clock_time(t1)

!     remove double samples
         K = 0
         newnode = 0
         do i = 1, NS
            if (XS(I) /= XYMIS .and. ZS(I) /= DMISS) then
               K = K + 1
               XS(K) = XS(I)
               YS(K) = YS(I)
               ZS(K) = ZS(I)
               newnode(i) = k ! old-to-new sample number
            end if
         end do

         call wall_clock_time(t2)

!     update permutation array
         k = 0
         do i = 1, NS
            j = IPSAM(i) ! old node number
            if (newnode(j) > 0) then
               k = k + 1
               IPSAM(k) = newnode(j)
            end if
         end do

         call wall_clock_time(t3)

!     set new number of samples
         NS = K

         if (JADOUBLE == 1) goto 5

         NUMWEG = NSORG - K
         if (NUMWEG >= 1) then
            write (OUD, '(I8)') NUMWEG
            ! CALL QNERROR('NUMBER OF DOUBLE POINTS REMOVED',OUD,' ')
         end if

         call wall_clock_time(t4)

!     output message
         if (jakdtree == 1) then
            txt = ''
            write (txt, "('merged ', I0, ' samples in ', F0.2, ' seconds.')") nummerged, t4 - t0
            call mess(LEVEL_INFO, trim(txt))
         end if

1234     continue

!     deallocate
         if (allocated(newnode)) deallocate (newnode)

         if (jakdtree == 1) then
!         deallocate permutation array
            if (allocated(iperm)) deallocate (iperm)

!         deallocate kdtree
            if (treeglob%itreestat /= ITREE_EMPTY) call delete_kdtree2(treeglob)
         end if

!     restore jsferic
         jsferic = jsferic_store

         return
      end
