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

module m_reabnd2pol
   use m_bndpoint2pol, only: bndpoint2pol
   use m_writeset, only: writeset
   use m_readset, only: readset

   implicit none

contains

   subroutine reabnd2pol(mbnd, mbca) ! convert d3d boundaryes stuf to model independent
      use precision, only: dp
      use m_grid
      use m_polygon
      use M_MISSING
      implicit none

      character :: rec * 132, fnam * 20
      integer, allocatable :: ma(:), na(:), mb(:), nb(:)
      integer :: mmx = 1000, k = 0, mbnd, mbca
      character(len=132) :: a(100)
      integer :: i, j
      integer :: kx, nra, kd, ku, kk, nr
      real(kind=dp) :: x1, x2, x3, x4

      allocate (ma(mmx), na(mmx), mb(mmx), nb(mmx))

      if (allocated(ijyes)) deallocate (ijyes)
      allocate (ijyes(mc + 1, nc + 1)); ijyes = 0

      do I = 2, MC ! set up flow oriented ijyes array, sorry for the inconvenience
         do J = 2, NC
            X1 = Xc(I - 1, J - 1)
            X2 = Xc(I, J - 1)
            X3 = Xc(I, J)
            X4 = Xc(I - 1, J)
            if (X1 /= XYMIS .and. X2 /= XYMIS .and. &
                X3 /= XYMIS .and. X4 /= XYMIS) IJYES(I, J) = 1
         end do
      end do

10    read (mbnd, '(a)', end=666) rec
      k = k + 1
      read (rec(25:), *) ma(k), na(k), mb(k), nb(k)
      goto 10

666   continue

      kx = k; nra = 0
      do k = 1, kx
         kd = max(1, k - 1); ku = min(kx, k + 1)

         if (mbca > 0) then
            kk = 9
            call readset(kk + 1, mbca, a) ! ; call readset(kk,mbca, b)
         end if

         fnam = 'kham_0001.cmp'

         ! if (k==1 .or. ma(k).ne.mb(kd) .and. na(k).ne.nb(kd) ) then
         call bndpoint2pol(ma(k), na(k))
         if (mbca > 0) then
            nr = nr + 1; call writeset(kk, fnam, nr, a)
         end if
         ! endif

         ! call bndpoint2pol( mb(k), nb(k) )
         ! if (mbca > 0) then
         !    nr = nr + 1 ; call writeset(kk,fnam,nr,b)
         ! endif

         ! if ( k.ne.kx .and. mb(k).ne.ma(ku) .and. nb(k).ne.na(ku) ) then
         !    npl = npl + 1 ; xpl(npl) = dmiss; ypl(npl) = dmiss ; nr = 0
         ! endif

      end do

      deallocate (ma, na, mb, nb, ijyes)

      if (mbnd /= 0) call doclose(mbnd)
      if (mbca /= 0) call doclose(mbca)

      return
   end subroutine reabnd2pol

end module m_reabnd2pol
