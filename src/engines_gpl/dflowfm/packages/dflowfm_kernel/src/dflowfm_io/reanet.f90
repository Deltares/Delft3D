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
! It seems it is ot used.
module m_reanet

   implicit none

   private

   public :: reanet

contains

   subroutine REANET(filename, k0, L0, NUMKN, NUMLN, istat)
      use precision, only: dp
      use m_netw
      use gridoperations
      use m_readyy
      use m_qn_read_error
      use m_qn_eof_error
      implicit none

      character(len=*), intent(in) :: filename !< inderdaad, filename
      integer :: k0, L0, NUMKN, NUMLN, istat

      integer :: MNET, JA, LMOD, KMOD
      integer :: k, nr, knread, L, N1
      integer :: numbersonline
      real(kind=dp) :: af
      character REC * 332

      call oldfil(mnet, filename)

      JA = 1
      read (MNET, '(A)', end=777, err=707) REC
      N1 = index(REC, '=') + 1
      read (REC(N1:), *, end=555, err=555) NUMKN

      read (MNET, '(A)') REC
      N1 = index(REC, '=') + 1
      read (REC(N1:), *, end=444, err=444) NUMLN
      read (MNET, '(A)') REC

      call readyy('reanet', 0d0)

      call INCREASENETW(K0 + NUMKN, L0 + NUMLN)

      call readyy('reanet', 0.05d0)

      KMOD = max(1, NUMK / 100)
      do K = K0 + 1, K0 + NUMKN
         if (mod(k, KMOD) == 0) then
            af = 0.05d0 + 0.45d0 * dble(k - 1 - K0) / dble(numkn)
            call readyy('reanet', af)
         end if
         read (MNET, '(A)', err=888, end=777) REC
         nr = numbersonline(rec)
         if (nr == 3) then
            read (REC, *, ERR=999) XK(K), YK(K), ZK(K)
         else
            read (REC, *, ERR=999) XK(K), YK(K)
            ZK(K) = ZKUNI
         end if
      end do

      read (MNET, *) rec

      LMOD = max(1, NUMLn / 1000)
      do L = L0 + 1, L0 + NUMLN
         if (mod(l, LMOD) == 0) then
            af = 0.5d0 + 0.5d0 * dble(l - 1) / dble(numln)
            call readyy('reanet', af)
         end if
         read (MNET, '(A)', end=777) REC

         nr = numbersonline(rec)
         if (nr == 3) then
            read (REC, *, ERR=888) KN(1, L), KN(2, L), KNREAD
         else
            read (REC, *, ERR=888) KN(1, L), KN(2, L)
            KNREAD = 2
         end if
         KN(1, L) = KN(1, L) + K0
         KN(2, L) = KN(2, L) + K0
         ! IF (KNREAD .NE. 1) KNREAD = 2
         KN(3, L) = KNREAD
      end do

666   continue
      istat = 0
      call DOCLOSE(MNET)

      call readyy('reanet', -1d0)

      netstat = NETSTAT_CELLS_DIRTY

      xkmin = minval(xk(1:numk))
      xkmax = maxval(xk(1:numk))

      return

999   call QNREADERROR('READING NETNODES, BUT GETTING ', REC, MNET)
      return

888   call QNREADERROR('READING NETLINKS, BUT GETTING ', REC, MNET)
      return

707   call QNREADERROR('READING NET FILE, GOT UNEXPECTED CONTENT ', REC, MNET)
      return

777   call QNEOFERROR(MNET)
      return

555   call QNREADERROR('READING NR OF NETNODES, BUT GETTING ', REC, MNET)
      return

444   call QNREADERROR('READING NR OF NETLINKS, BUT GETTING ', REC, MNET)
      return

   end subroutine REANET

end module m_reanet
