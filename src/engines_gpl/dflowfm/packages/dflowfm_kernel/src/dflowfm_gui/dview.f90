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

module m_dview

   implicit none

contains

   subroutine DVIEW(XD, YD, ZD, X, Y, Z)
      use precision, only: dp
      use m_missing, only: dmiss
      use m_viewmat

      real(kind=dp) :: ce
      integer :: i
      ! GEEF perspectievische COORDINATEN
      ! xD,yD,zD                             :coordinaten te tekenen punt
      ! x0s,y0s                              :waar op scherm ligt kijklijn
      ! X,Y,Z                                :scherm coordinaten
      ! Vs                                   :viewing matrix na viema

      real(kind=dp) XD, YD, ZD, X, Y, Z
      dimension CE(4)
      ! use z as zd temporarily (zet to zero when zd==dmiss)
      if (zd == dmiss) then
         z = 0
      else
         z = zd
      end if
      do I = 1, 3
         CE(I) = VS(I, 1) * XD + VS(I, 2) * YD + VS(I, 3) * Z + VS(I, 4)
      end do
      Z = CE(3)
      if (Z < 0) then
         Z = dmiss
      else
         X = CE(1) / Z + X0S
         Y = CE(2) / Z + Y0S
      end if
   end subroutine DVIEW

end module m_dview
