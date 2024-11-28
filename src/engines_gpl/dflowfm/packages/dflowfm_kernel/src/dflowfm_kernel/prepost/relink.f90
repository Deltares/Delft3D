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

   subroutine RELINK()

      use m_dellinksinpol, only: dellinksinpol
      use m_netw
      use m_ec_triangle
      use gridoperations
      use m_polygon
      use gridoperations
      use m_readyy
      use m_set_nod_adm

      use m_ec_basic_interpolation, only: dlaun

      implicit none

      double precision :: af
      integer :: ierr
      integer :: ja
      integer :: k
      integer :: k1
      integer :: k1l
      integer :: k2
      integer :: k2l
      integer :: ki
      integer :: l
      integer :: ll
      integer :: n
      integer :: n1
      integer :: n2
      integer :: new
      integer :: nn

      integer, allocatable :: KIN(:)
      double precision, allocatable :: X(:), Y(:)

      allocate (KIN(NUMK), X(NUMK), Y(NUMK), STAT=IERR)
      call AERR('KIN(NUMK), X(NUMK), Y(NUMK)', IERR, 3 * NUMK)

      call DSELECTINP(XK, YK, NUMK, KIN)

      KI = 0
      do K = 1, NUMK
         if (KIN(K) > 0) then
            KI = KI + 1
            X(KI) = XK(K)
            Y(KI) = YK(K)
            KIN(KI) = K
         end if
      end do

      call READYY('TRIANGULATING', 0d0)

      call DLAUN(X, Y, KI, 1, ierr)

      call READYY('TRIANGULATING', 0.3d0)

      call DELLINKSINPOL()

      L = NUML
      do N = 1, NUMTRI
         AF = 0.3d0 + 0.7d0 * dble(N) / dble(NUMTRI)
         call READYY('TRIANGULATING', AF)

         JA = 1
         ! CALL CHECKTRIANGLE(N,JA)
         if (JA == 0) then
            cycle
         end if
         do NN = 1, 3
            N1 = NN; N2 = N1 + 1; if (N1 == 3) N2 = 1
            K1 = INDX(N1, N); K2 = INDX(N2, N)
            K1 = KIN(K1); K2 = KIN(K2)

            NEW = 1
            do LL = NUML, 1, -1
               K1L = KN(1, LL); K2L = KN(2, LL)
               if (K1 == K1L .and. K2 == K2L .or. &
                   K2 == K1L .and. K1 == K2L) then
                  NEW = 0; exit
               end if
            end do

            if (NEW == 0) cycle

            L = L + 1; 
            if (L > LMAX) then
               call INCREASENETW(int(1.2 * NUMK), int(1.2 * NUML))
            end if
            NUML = L
            KN(1, L) = K1; KN(2, L) = K2

         end do
      end do

      deallocate (KIN)

      call SETNODADM(1)

      call READYY('TRIANGULATING', -1d0)

      return
   end subroutine RELINK
