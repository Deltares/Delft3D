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

module m_teknetstuff
   use m_tekpreviousnet
   use m_teknodevals
   use m_teknodenums

   implicit none

contains

   subroutine TEKNETSTUFF(key)
      use precision, only: dp
      use m_teknetcells
      use m_teknet
      use m_teklinkvals
      use m_teklinknums
      use unstruc_colors
      use unstruc_display, only: jaHighlight
      use m_netw
      use m_drawthis
      use m_gtext
      implicit none
      real(kind=dp) :: XP, YP
      integer :: key, K1, K2

      if (NDRAW(7) >= 2) call TEKLINKVALS(NDRAW(11))

      if (NDRAW(8) >= 2) call TEKNODEVALS(NDRAW(19))

      call TEKNET(key)

      call TEKPREVIOUSNET(NCOLRN)

      if (NDRAW(7) >= 2) call TEKLINKNUMS(NDRAW(11), NCOLLN)

      if (NDRAW(8) >= 2) call TEKNODENUMS(NDRAW(19), NCOLDN)

      call TEKNETCELLS(NDRAW(33), 0, 1)

      if (jaHighlight == 1) then
         if (nOdmax /= 0) then
            call gtext('NETNODMax', xK(nOdmax), yK(nOdmax), 31)
         end if
         if (nOdmin /= 0) then
            call gtext('NETNODMin', xK(nOdmin), yK(nOdmin), 221)
         end if
         if (LINmax /= 0) then
            K1 = KN(1, LINMAX)
            K2 = KN(2, LINMAX)
            if (K1 /= 0 .and. K2 /= 0) then
               XP = 0.5d0 * (XK(K1) + XK(K2))
               YP = 0.5d0 * (YK(K1) + YK(K2))
            end if
            call gtext('NETLINMax', XP, YP, 31)
         end if
         if (LINmin /= 0) then
            K1 = KN(1, LINMIN)
            K2 = KN(2, LINMIN)
            if (K1 /= 0 .and. K2 /= 0) then
               XP = 0.5d0 * (XK(K1) + XK(K2))
               YP = 0.5d0 * (YK(K1) + YK(K2))
               call gtext('NETLINMin', XP, YP, 221)
            end if
         end if
         if (netcelmax /= 0) then
            call gtext('NETcelmax', xzw(netcelmax), yzw(netcelmax), 31)
         end if
         if (netcelmin /= 0) then
            call gtext('NETcelmin', xzw(netcelmin), yzw(netcelmin), 221)
         end if
      end if

      return
   end subroutine TEKNETSTUFF

end module m_teknetstuff
