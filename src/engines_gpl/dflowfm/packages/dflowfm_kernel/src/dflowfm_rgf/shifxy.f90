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

      !> Operates on active grid from m_grid directly!
      subroutine SHIFXY(IS, JS, MP, NP)

         !     XH,     YH,     mmax, nmax, MC,     NC, IS,     JS,     MP,     NP        )
         use m_missing
         use m_grid
         use geometry_module, only: pinpok

         implicit none
         integer :: is, js, mp, np

         integer :: i, j
!     schuif data naar rechts of boven of beide en geef nieuwe MC,NC

         MC = MC + IS
         NC = NC + JS

         call increasegrid(mc, nc)

         MP = MP + IS
         NP = NP + JS

         do J = NC, 1 + JS, -1
            do I = MC, 1 + IS, -1
               Xc(I, J) = Xc(I - IS, J - JS)
               Yc(I, J) = Yc(I - IS, J - JS)
               Zc(I, J) = Zc(I - IS, J - JS)
            end do
         end do
         if (IS == 1) then
            do J = 1, NC
               Xc(1, J) = XYMIS
               Yc(1, J) = XYMIS
               Zc(1, J) = XYMIS
            end do
         end if
         if (JS == 1) then
            do I = 1, MC
               Xc(I, 1) = XYMIS
               Yc(I, 1) = XYMIS
               Zc(I, 1) = XYMIS
            end do
         end if
         return
      end subroutine shifxy
