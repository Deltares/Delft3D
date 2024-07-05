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

      SUBROUTINE RMDOUBLE(XS,YS,ZS,IPSAM,NS)
      use m_missing
      use m_sferic
      use unstruc_messages
      use kdtree2Factory

      implicit none

      integer :: i
      integer :: j
      integer :: jadouble
      integer :: k
      integer :: ns
      integer :: nsorg
      integer :: numweg
      integer :: isam, jsam, ksam
      double precision,       intent(inout) :: XS(NS), YS(NS), ZS(NS)
      integer, dimension(NS), intent(inout) :: IPSAM  !< permutation array (increasing x-coordinate)
      integer, dimension(:), allocatable    :: newnode

      double precision, dimension(:), allocatable :: xx, yy  ! non-missing sample coordinates

      integer,          dimension(:), allocatable :: iperm   ! permutation array

      double precision                            :: t0, t1, t2, t3, t4
      integer                                     :: ii, jj, NN, num, nummerged, ierror
      integer                                     :: jakdtree=1

      integer                                     :: jsferic_store

      character(len=128)                          :: txt

      double precision, parameter                 :: dtol2 = 1d-8 ! sample-on-top of each other tolerance, squared

      CHARACTER OUD*8, NIEUW*8
      NSORG = NS

      allocate(newnode(NS))

!     store jsferic
      jsferic_store = jsferic

!     safety
      !if ( NS.lt.100 ) then
      !   jakdtree=0
      !end if

      call klok(t0)

      if ( jakdtree.eq.1 ) then
!        force Cartesian coordinates
         jsferic = 0
!        get non-missing sample coordinates
         allocate(xx(NS))
         xx = 0d0
         allocate(yy(NS))
         yy = 0d0
         allocate(iperm(NS))
         iperm = 0

!        get non-missing sample coordinates
         num = 0
         do i=1,NS
            if ( xs(i).ne.DMISS .and. ys(i).ne.DMISS ) then
               num = num+1
               xx(num) = xs(i)
               yy(num) = ys(i)
               iperm(num) = i
            end if
         end do

!        initialize kdtree
         call build_kdtree(treeglob,num,xx,yy,ierror, jsferic, dmiss)

!        deallocate arrays with non-missing node coordinates
         deallocate(xx)
         deallocate(yy)

         if ( ierror.ne.0 ) then
!           deallocate permutation array
            if ( allocated(iperm) ) deallocate(iperm)

!           deallocate kdtree
            if ( treeglob%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeglob)

!           disable kdtree
            jakdtree = 0
         end if
      end if

      nummerged=0

!     fill double samples with DMISS
    5 CONTINUE
      JADOUBLE = 0

      if ( jakdtree.eq.1 ) then
!        find samples on top of each other
         do ii=1,num
            i=iperm(ii)

            if ( i.eq.0 ) cycle   ! already merged

!           fill query vector
            call make_queryvector_kdtree(treeglob,xs(i),ys(i), jsferic)

!           count number of points in search area
            NN = kdtree2_r_count(treeglob%tree,treeglob%qv,dtol2)

            if ( NN.gt.1 ) then ! at least two samples need to be merged
!              resize results array if necessary
               call realloc_results_kdtree(treeglob,NN)

!              find other nodes
               call kdtree2_n_nearest(treeglob%tree,treeglob%qv,NN,treeglob%results)

!              merge with other nodes
               do k=1,NN
                  jj = treeglob%results(k)%idx
                  j  = iperm(jj)
!                 exclude own sample and samples already deleted
                  if ( j.ne.i .and. j.gt.0 ) then
                     if ( xs(i).eq.xs(j) .and. ys(i).eq.ys(j) ) then ! NOT SPHERICAL-PROOF
                        iperm(jj) = 0
                        xs(j) = DMISS
                        jadouble = 1
                        nummerged = nummerged+1
                     end if
                  end if
               end do
            end if
         end do
      else  !  non kdtree
      do I = 1,NS-1
         ISAM = IPSAM(I)
         IF (XS(ISAM) .NE. XYMIS .and. ZS(ISAM) .NE. DMISS) THEN
            J = I
   15       CONTINUE
            IF (J .LT. NS) THEN
               J = J + 1
               JSAM = IPSAM(J)
               IF (XS(ISAM) .EQ. XS(JSAM)) THEN
                  IF (YS(ISAM) .EQ. YS(JSAM)) THEN
                      XS(JSAM)    = XYMIS
                      JADOUBLE = 1
                  ENDIF
                  GOTO 15
               ENDIF
            ENDIF
         ENDIF
end do

      end if

      call klok(t1)

!     remove double samples
      K = 0
      newnode = 0
      do i = 1,NS
         IF (XS(I) .NE. XYMIS .and. ZS(I) .NE. DMISS) THEN
            K = K + 1
            XS(K) = XS(I)
            YS(K) = YS(I)
            ZS(K) = ZS(I)
            newnode(i) = k   ! old-to-new sample number
         ENDIF
end do

      call klok(t2)

!     update permutation array
      k = 0
      do i=1,NS
         j = IPSAM(i)   ! old node number
         if ( newnode(j).gt.0 ) then
            k=k+1
            IPSAM(k) = newnode(j)
         end if
      end do

      call klok(t3)

!     set new number of samples
      NS = K

      IF (JADOUBLE .EQ. 1) GOTO 5

      NUMWEG = NSORG - K
      IF (NUMWEG .GE. 1) THEN
         WRITE(OUD,'(I8)') NUMWEG
         ! CALL QNERROR('NUMBER OF DOUBLE POINTS REMOVED',OUD,' ')
      ENDIF

      call klok(t4)

!     output message
      if ( jakdtree.eq.1 ) then
         txt = ''
         write(txt, "('merged ', I0, ' samples in ', F0.2, ' seconds.')") nummerged, t4-t0
         call mess(LEVEL_INFO, trim(txt))
      end if

 1234 continue

!     deallocate
      if ( allocated(newnode) ) deallocate(newnode)

      if ( jakdtree.eq.1 ) then
!         deallocate permutation array
          if ( allocated(iperm) ) deallocate(iperm)

!         deallocate kdtree
          if ( treeglob%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeglob)
      end if

!     restore jsferic
      jsferic = jsferic_store

      RETURN
      END
