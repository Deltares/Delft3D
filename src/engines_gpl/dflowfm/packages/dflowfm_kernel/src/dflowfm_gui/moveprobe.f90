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

!>    move probe:
!>      7 8 9
!>      4 5 6
!>      1 2 3
module m_moveprobe

   implicit none

contains

   subroutine moveprobe(idir, kk, xp, yp)
      use precision, only: dp
      use m_flowgeom, only: ln, nd, csu, snu
      use network_data, only: xzw, yzw

      integer, intent(in) :: idir !< direction (see keys on keypad)
      integer, intent(inout) :: kk !< probed flownode number
      real(kind=dp), intent(inout) :: xp, yp !< probed flownode coordinates

      real(kind=dp) :: csdir, sndir !< direction vector components
      real(kind=dp) :: dum
      real(kind=dp) :: dmaxinprod

      integer :: i, L, knext

      if (kk == 0 .or. idir == 5) then
         call in_flowcell(xp, yp, KK)
      else
!           determine direction vector
         csdir = mod(idir - 1, 3) - 1d0
         sndir = int((idir - 1) / 3) - 1d0
         dum = sqrt(csdir**2 + sndir**2)
         csdir = csdir / dum
         sndir = sndir / dum

!!           find next flownode
!            knext = 0
!            dmaxinprod = -huge(0d0)
!
!            do i=1,size(nd(kk)%nod)
!               k = nd(kk)%nod(i)
!               do j=1,cn(k)%lnx
!                  L = abs(cn(k)%ln(j))
!                  do jj=1,2
!                     k2 = ln(jj,L)
!                     if ( k2.eq.kk ) cycle
!
!                     call getdxdy(xzw(kk),yzw(kk),xzw(k2),yzw(k2),cs,sn)
!                     dum = sqrt(cs**2+sn**2)
!                     cs = cs/dum
!                     sn = sn/dum
!
!                     dum = csdir*cs + sndir*sn
!
!                     if ( dum.gt.0d0 .and. dum.gt.dmaxinprod) then
!                        knext = k2
!                        dmaxinprod = dum
!                     end if
!
!                  end do
!               end do
!            end do

!           find next flownode
         knext = 0
         dmaxinprod = -huge(0d0)
         do i = 1, nd(kk)%lnx
            L = nd(kk)%ln(i)
            if (L < 0) then
               dum = csdir * csu(-L) + sndir * snu(-L)
            else
               dum = -(csdir * csu(L) + sndir * snu(L))
            end if
            if (dum > 0d0 .and. dum > dmaxinprod) then
               knext = ln(1, abs(L)) + ln(2, abs(L)) - kk
               dmaxinprod = dum
            end if
         end do

         if (knext /= 0) then
            kk = knext
            xp = xzw(kk)
            yp = yzw(kk)
         end if
      end if

      return
   end subroutine moveprobe

end module m_moveprobe
