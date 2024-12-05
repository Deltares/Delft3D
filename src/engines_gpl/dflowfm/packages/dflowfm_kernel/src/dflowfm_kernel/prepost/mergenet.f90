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

   subroutine MERGENET()
      use precision, only: dp
      use m_netw
      use M_MERGENET ! NUMM JBLUNT
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      use gridoperations
      use m_mergenodes
      use m_set_nod_adm

      implicit none
      real(kind=dp) :: eps
      integer :: ierr
      integer :: in1
      integer :: j
      integer :: ja
      integer :: k, kk, k1, k2
      integer :: l
      integer :: numj
      integer, allocatable :: KM(:)

      logical :: lboundnode ! true if netnode is on boundary
      logical :: jamerged ! Whether or not any merge operations were performed.

      allocate (KM(KMAX), STAT=IERR)
      KM = 0
      jamerged = .false.

!   CALL MERGENETPARAMETERS()

!   J  = 0
!   DO K = 1, NUMK
!      CALL DPINPOK( XK(K), YK(K), ZK(K), NPL, XPL, YPL, IN1)
!      IF (IN1 .EQ. 1) THEN
!         IF (NMK(K) .LE. NUMM) THEN
!            J = J + 1 ; KM(J) = K
!         ENDIF
!      ENDIF
!   ENDDO

      j = 0
      in1 = -1
      do k = 1, numk
!      CALL DPINPOK( XK(K), YK(K), ZK(K), NPL, XPL, YPL, IN1)
!      IF (IN1 .EQ. 1) THEN
         lboundnode = .false.

         do k1 = 1, nmk(k)
            L = nod(k)%lin(k1)
            if (lnn(L) == 1) then
               lboundnode = .true.
               exit
            end if
         end do

         if (lboundnode) then
            j = j + 1
            km(j) = k
         end if
!      ENDIF
      end do

      EPS = 0.01d0
      NUMJ = J
      do K = 1, NUMJ - 1
         K1 = KM(K)
         if (K1 >= 1) then
            eps = 1d9
            do kk = 1, nmk(k1)
               L = nod(k1)%lin(kk)
!            if (lnn(L) == 1) then     ! SPvdP: this gives problems
               if (kn(1, L) < 1 .or. kn(2, L) < 1) cycle
               eps = min(eps, dbdistance(XK(kn(1, L)), YK(kn(1, L)), XK(kn(2, L)), YK(kn(2, L)), jsferic, jasfer3D, dmiss))
!            endif
            end do
            if (eps == 1d9) eps = 0d0 ! no links considered => eps=0
            do L = K + 1, NUMJ
               K2 = KM(L)
               if (K2 >= 1) then
                  ! Note that mergenet merges boundary net nodes.
                  ! Not only in two disjoint net parts, but also two neighbouring net nodes
                  ! at a boundary may be merged. (i.e., gridtonet of a curvigrid may produce
                  ! a net with triangles.)
                  if (dbdistance(XK(K1), YK(K1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss) < 0.25d0 * eps) then
                     call MERGENODES(K1, K2, JA)
                     KM(K) = 0; KM(L) = 0
                     jamerged = .true.
                  end if
               end if
            end do
         end if
      end do

      if (jamerged) then
         call setnodadm(0)
      end if

      return
   end subroutine MERGENET
