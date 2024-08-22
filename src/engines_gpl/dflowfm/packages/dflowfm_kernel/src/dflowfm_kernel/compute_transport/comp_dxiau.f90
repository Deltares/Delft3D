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

! compute Au/Dx for diffusive flux
subroutine comp_dxiAu() ! or: setdxiau
   use m_flowgeom, only: ln, Lnx, dxi, wu, Lnxi, bob
   use m_flow, only: hs, zws, kmx, Au, hu, jadiffusiononbnd, chkdifd, ifixedweirscheme, s1
   use m_fixedweirs, only: nfxw, lnfxw
   use m_transport, only: dxiAu, jalimitdtdiff
   use timers, only: timon, timstrt, timstop

   implicit none

   integer :: k1, k2, i
   integer :: LL, L, Lb, Lt
   double precision :: hh, ff
   integer(4) :: ithndl =  0
   
   if (timon) call timstrt("comp_dxiAu", ithndl)

   if (jalimitdtdiff == 0) then
      if (kmx == 0) then
         do L = 1, Lnx
            dxiAu(L) = dxi(L) * Au(L)
         end do
      else
         do LL = 1, Lnx
            call getLbotLtop(LL, Lb, Lt)
            do L = Lb, Lt
               dxiAu(L) = dxi(LL) * Au(L)
            end do
         end do
      end if
   else
      if (kmx == 0) then
         do L = 1, Lnx
            if (au(L) > 0d0) then
               k1 = ln(1, L)
               k2 = ln(2, L)
               hh = min(hs(k1), hs(k2), hu(L))
               if (chkdifd > 0 .and. hh < chkdifd) then
                  hh = hh * hh / chkdifd
               end if
               dxiAu(L) = dxi(L) * wu(L) * hh
            else
               dxiAu(L) = 0d0
            end if
         end do
      else
         do LL = 1, Lnx
            if (au(LL) > 0d0) then
               call getLbotLtop(LL, Lb, Lt)
               do L = Lb, Lt
                  if (au(L) > 0d0) then
                     k1 = ln(1, L)
                     k2 = ln(2, L)
                     hh = min(zws(k1) - zws(k1 - 1), zws(k2) - zws(k2 - 1), hu(L) - hu(L - 1))
                     if (chkdifd > 0 .and. hh < chkdifd) then
                        hh = hh * hh / chkdifd
                     end if
                     dxiAu(L) = dxi(LL) * wu(LL) * hh
                  else
                     dxiAu(L) = 0d0
                  end if
               end do
            end if
         end do
      end if
   end if

   if (ifixedweirscheme >= 7 .and. ifixedweirscheme <= 9) then ! reduce diff. area's in tabelb and villemonte that diffused through dikes originally
      do i = 1, nfxw
         L = lnfxw(i)
         if (L > 0) then
            if (au(L) > 0d0) then
               k1 = ln(1, L); k2 = ln(2, L)
               hh = max(s1(k1), s1(k2)) - max(bob(1, L), bob(2, L))
               if (hh > 0) then
                  ff = wu(L) * hh / au(L)
                  call getLbotLtop(L, Lb, Lt)
                  do LL = Lb, Lt
                     dxiAu(LL) = dxiAu(LL) * ff
                  end do
               end if
            end if
         end if
      end do
   end if

   if (jadiffusiononbnd == 0) then
      do LL = lnxi + 1, lnx
         call getLbotLtop(LL, Lb, Lt)
         do L = Lb, Lt
            dxiAu(L) = 0d0
         end do
      end do
   end if

   if (timon) call timstop(ithndl)
   return
end subroutine comp_dxiAu
