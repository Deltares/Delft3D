!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_tekship

   implicit none

   private

   public :: tekship

contains

   subroutine tekship()
      use precision, only: dp
      use m_ship, only: iniship, nshiptxy, shi, fx2, fy2, fm2, shl, fricx, fricy, fricm, stuwx, stuwy, stuwm, shu, shv, sho, shx, shy, shb, roer
      use m_set_col, only: setcol
      use m_movabs, only: movabs
      use m_lnabs, only: lnabs
      use m_slnabs, only: slnabs
      use m_smovabs, only: smovabs
      use m_shtext, only: shtext

      real(kind=dp) :: sx2, sy2, css, sns, rr, cr, sr, snum
      integer :: n
      if (iniship == 0) then
         return
      end if

      call setcol(4)

      do n = 1, nshiptxy
         css = cos(shi(n))
         sns = sin(shi(n))

         call smovabs(n, 1.0_dp, 0.0_dp)
         call slnabs(n, 0.9_dp, -1.0_dp)
         call slnabs(n, -1.0_dp, -1.0_dp)
         call slnabs(n, -1.0_dp, 1.0_dp)
         call slnabs(n, 0.9_dp, 1.0_dp)
         call slnabs(n, 1.0_dp, 0.0_dp)

         snum = css * fx2(n) + sns * fy2(n) ! pressure force in shipL dir
         call shtext(n, snum, -1.3_dp, 0.0_dp)

         snum = -sns * fx2(n) + css * fy2(n) ! pressure force in shipB dir
         call shtext(n, snum, -1.3_dp, 1.0_dp)

         snum = fm2(n) / shL(n) ! pressure mom vertical ax
         call shtext(n, snum, -1.3_dp, -1.0_dp)

         snum = css * fricx(n) + sns * fricy(n) ! fric force in shipL dir
         call shtext(n, snum, 1.3_dp, 0.0_dp)

         snum = -sns * fricx(n) + css * fricy(n) ! fric force in shipB dir
         call shtext(n, snum, 1.3_dp, 1.0_dp)

         snum = fricm(n) / shL(n) ! fric mom vertical ax
         call shtext(n, snum, 1.3_dp, -1.0_dp)

         snum = css * stuwx(n) + sns * stuwy(n) ! stuwforce in shipL dir
         call shtext(n, snum, -0.8_dp, 0.0_dp)

         snum = -sns * stuwx(n) + css * stuwy(n) ! stuwforce in shipB dir
         call shtext(n, snum, -0.8_dp, 1.0_dp)

         snum = stuwm(n) / shL(n) ! stuwmom vertical ax normalised by half length
         call shtext(n, snum, -0.8_dp, -1.0_dp)

         snum = css * shu(n) + sns * shv(n) ! snelheid in shipL dir
         call shtext(n, snum, -0.0_dp, 0.0_dp)

         snum = -sns * shu(n) + css * shv(n) ! snelheid in shipB dir
         call shtext(n, snum, -0.0_dp, 1.1_dp)

         snum = sho(n) ! ronjes/minuut vertical ax
         call shtext(n, snum * 60.0_dp / 6.28_dp, -0.0_dp, -1.1_dp)

         sx2 = shx(n) - shL(n) * css ! rudder
         sy2 = shy(n) - shL(n) * sns
         call movabs(sx2, sy2)
         rr = 0.4_dp * shb(n)
         cr = cos(shi(n) + roer(n))
         sr = sin(shi(n) + roer(n))
         call lnabs(sx2 - rr * cr, sy2 - rr * sr)

      end do
   end subroutine tekship

end module m_tekship
