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

module m_readyzprofnr

   implicit none

   private

   public :: readyzprofnr

contains

   !> Reads xyz profiles (and stores them as yz profiles).
 !! When the model is in spherical coordinates, the x,y pairs (lon,lat) are translated
 !! into metric distances.
   subroutine readyzprofnr(myzprofs, iprofnr, nyz, yy, zz, mx, width, height, zmin)
      use precision, only: dp
      use unstruc_messages
      use m_profiles, only: tolzprof, ntolsave
      use geometry_module, only: dbdistance
      use m_sferic, only: jsferic, jasfer3D
      use m_missing
      implicit none
      integer :: myzprofs, iprofnr, nyz, mx
      real(kind=dp) :: yy(mx), zz(mx), width, height, zmin

      integer :: L, nr, n, ikp(9999), n0, n1, n2, nn
      character(len=256) :: rec, tex
      real(kind=dp) :: xx0, yy0, zz0, xx1, yy1, zz1, zmax, a, b, dif, zn1, y01, y02, dy

      nyz = 0

      ! rewind (myzprofs)
10    read (myzprofs, '(a)', end=999) rec
      if (index(rec, 'PROFNR') > 0) then
         L = index(rec, '=') + 1
         read (rec(L:), *) nr
         if (nr == iprofnr) then
            read (myzprofs, '(a)', end=999) rec
            read (rec, *, err=888) nyz

            read (myzprofs, '(a)', end=999) rec
            read (rec, *, err=777) xx0, yy0, zz0
            yy(1) = 0d0
            zz(1) = zz0
            zmin = zz0

            nn = 1
            do n = 2, nyz
               read (myzprofs, '(a)', end=999) rec
               read (rec, *, err=777) xx1, yy1, zz1

               !         read(myzprofs,*,err=777) xx1, yy1, zz1
               dy = dbdistance(xx0, yy0, xx1, yy1, jsferic, jasfer3D, dmiss)
               !dy = sqrt( (xx1-xx0)**2 + (yy1-yy0)**2)
               if (dy > 0d0) then
                  nn = nn + 1
                  yy(nn) = yy(nn - 1) + dy
                  zz(nn) = zz1
                  xx0 = xx1
                  yy0 = yy1
               else
                  write (msgbuf, '(a,i0,a,i0,a)') ' While reading PROFNR=', nr, ': point #', n, ' discarded, because it is the same as previous point.'
                  call warn_flush()
               end if

               zmin = min(zmin, zz1)
            end do
            nyz = nn ! Some points may have been discarded
            zmax = -9d9
            do n = 1, nyz
               zz(n) = zz(n) - zmin
               zmax = max(zmax, zz(n))
            end do
            height = zmax
            width = yy(nn)

            if (nyz > 2) then ! throw away points that can be represented by linear interpolation
               ikp(1) = 1
               ikp(nyz) = 1
               n0 = 1
               do n1 = 2, nyz - 1
                  n2 = n1 + 1
                  if ((zz(n1) - zz(n0)) * (zz(n2) - zz(n1)) < 0) then ! do not touch local maxima or minima
                     ikp(n1) = 1
                     n0 = n1
                  else
                     y02 = yy(n2) - yy(n0)
                     y01 = yy(n1) - yy(n0)
                     b = y01 / y02; a = 1d0 - b
                     zn1 = a * zz(n0) + b * zz(n2)
                     dif = abs(zz(n1) - zn1)
                     if (dif > tolzprof) then
                        ikp(n1) = 1
                        n0 = n1
                     else
                        ikp(n1) = 0
                     end if
                  end if
               end do
               n = 0
               do n1 = 1, nyz
                  if (ikp(n1) == 1) then
                     n = n + 1
                     yy(n) = yy(n1)
                     zz(n) = zz(n1)
                  end if
               end do
               ntolsave = ntolsave + nyz - n
               nyz = n
            end if

            return
         end if
      end if
      goto 10

999   write (tex, '(i10)') iprofnr
      call readerror('could not find profnr iprofnr = ', tex, myzprofs); return

888   call readerror('reading two integers nrows, nkols, but getting ', rec, myzprofs); return

777   call readerror('reading x,y,z , but getting ', rec, myzprofs); return

   end subroutine readyzprofnr

end module m_readyzprofnr
